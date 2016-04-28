{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.AddAttributesToFindings
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Assigns attributes (key and value pair) to the findings specified by the
-- findings\' ARNs.
module Network.AWS.Inspector.AddAttributesToFindings
    (
    -- * Creating a Request
      addAttributesToFindings
    , AddAttributesToFindings
    -- * Request Lenses
    , aatfFindingARNs
    , aatfAttributes

    -- * Destructuring the Response
    , addAttributesToFindingsResponse
    , AddAttributesToFindingsResponse
    -- * Response Lenses
    , aatfrsMessage
    , aatfrsResponseStatus
    ) where

import           Network.AWS.Inspector.Types
import           Network.AWS.Inspector.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'addAttributesToFindings' smart constructor.
data AddAttributesToFindings = AddAttributesToFindings'
    { _aatfFindingARNs :: ![Text]
    , _aatfAttributes  :: ![Attribute]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'AddAttributesToFindings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aatfFindingARNs'
--
-- * 'aatfAttributes'
addAttributesToFindings
    :: AddAttributesToFindings
addAttributesToFindings =
    AddAttributesToFindings'
    { _aatfFindingARNs = mempty
    , _aatfAttributes = mempty
    }

-- | The ARNs specifying the findings that you want to assign attributes to.
aatfFindingARNs :: Lens' AddAttributesToFindings [Text]
aatfFindingARNs = lens _aatfFindingARNs (\ s a -> s{_aatfFindingARNs = a}) . _Coerce;

-- | The array of attributes that you want to assign to specified findings.
aatfAttributes :: Lens' AddAttributesToFindings [Attribute]
aatfAttributes = lens _aatfAttributes (\ s a -> s{_aatfAttributes = a}) . _Coerce;

instance AWSRequest AddAttributesToFindings where
        type Rs AddAttributesToFindings =
             AddAttributesToFindingsResponse
        request = postJSON inspector
        response
          = receiveJSON
              (\ s h x ->
                 AddAttributesToFindingsResponse' <$>
                   (x .?> "message") <*> (pure (fromEnum s)))

instance Hashable AddAttributesToFindings

instance NFData AddAttributesToFindings

instance ToHeaders AddAttributesToFindings where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("InspectorService.AddAttributesToFindings" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON AddAttributesToFindings where
        toJSON AddAttributesToFindings'{..}
          = object
              (catMaybes
                 [Just ("findingArns" .= _aatfFindingARNs),
                  Just ("attributes" .= _aatfAttributes)])

instance ToPath AddAttributesToFindings where
        toPath = const "/"

instance ToQuery AddAttributesToFindings where
        toQuery = const mempty

-- | /See:/ 'addAttributesToFindingsResponse' smart constructor.
data AddAttributesToFindingsResponse = AddAttributesToFindingsResponse'
    { _aatfrsMessage        :: !(Maybe Text)
    , _aatfrsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'AddAttributesToFindingsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aatfrsMessage'
--
-- * 'aatfrsResponseStatus'
addAttributesToFindingsResponse
    :: Int -- ^ 'aatfrsResponseStatus'
    -> AddAttributesToFindingsResponse
addAttributesToFindingsResponse pResponseStatus_ =
    AddAttributesToFindingsResponse'
    { _aatfrsMessage = Nothing
    , _aatfrsResponseStatus = pResponseStatus_
    }

-- | Confirmation details of the action performed.
aatfrsMessage :: Lens' AddAttributesToFindingsResponse (Maybe Text)
aatfrsMessage = lens _aatfrsMessage (\ s a -> s{_aatfrsMessage = a});

-- | The response status code.
aatfrsResponseStatus :: Lens' AddAttributesToFindingsResponse Int
aatfrsResponseStatus = lens _aatfrsResponseStatus (\ s a -> s{_aatfrsResponseStatus = a});
