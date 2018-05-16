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
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Assigns attributes (key and value pairs) to the findings that are specified by the ARNs of the findings.
--
--
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
    , aatfrsResponseStatus
    , aatfrsFailedItems
    ) where

import Network.AWS.Inspector.Types
import Network.AWS.Inspector.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'addAttributesToFindings' smart constructor.
data AddAttributesToFindings = AddAttributesToFindings'
  { _aatfFindingARNs :: !(List1 Text)
  , _aatfAttributes  :: ![Attribute]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AddAttributesToFindings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aatfFindingARNs' - The ARNs that specify the findings that you want to assign attributes to.
--
-- * 'aatfAttributes' - The array of attributes that you want to assign to specified findings.
addAttributesToFindings
    :: NonEmpty Text -- ^ 'aatfFindingARNs'
    -> AddAttributesToFindings
addAttributesToFindings pFindingARNs_ =
  AddAttributesToFindings'
    {_aatfFindingARNs = _List1 # pFindingARNs_, _aatfAttributes = mempty}


-- | The ARNs that specify the findings that you want to assign attributes to.
aatfFindingARNs :: Lens' AddAttributesToFindings (NonEmpty Text)
aatfFindingARNs = lens _aatfFindingARNs (\ s a -> s{_aatfFindingARNs = a}) . _List1

-- | The array of attributes that you want to assign to specified findings.
aatfAttributes :: Lens' AddAttributesToFindings [Attribute]
aatfAttributes = lens _aatfAttributes (\ s a -> s{_aatfAttributes = a}) . _Coerce

instance AWSRequest AddAttributesToFindings where
        type Rs AddAttributesToFindings =
             AddAttributesToFindingsResponse
        request = postJSON inspector
        response
          = receiveJSON
              (\ s h x ->
                 AddAttributesToFindingsResponse' <$>
                   (pure (fromEnum s)) <*>
                     (x .?> "failedItems" .!@ mempty))

instance Hashable AddAttributesToFindings where

instance NFData AddAttributesToFindings where

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
  { _aatfrsResponseStatus :: !Int
  , _aatfrsFailedItems    :: !(Map Text FailedItemDetails)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AddAttributesToFindingsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aatfrsResponseStatus' - -- | The response status code.
--
-- * 'aatfrsFailedItems' - Attribute details that cannot be described. An error code is provided for each failed item.
addAttributesToFindingsResponse
    :: Int -- ^ 'aatfrsResponseStatus'
    -> AddAttributesToFindingsResponse
addAttributesToFindingsResponse pResponseStatus_ =
  AddAttributesToFindingsResponse'
    {_aatfrsResponseStatus = pResponseStatus_, _aatfrsFailedItems = mempty}


-- | -- | The response status code.
aatfrsResponseStatus :: Lens' AddAttributesToFindingsResponse Int
aatfrsResponseStatus = lens _aatfrsResponseStatus (\ s a -> s{_aatfrsResponseStatus = a})

-- | Attribute details that cannot be described. An error code is provided for each failed item.
aatfrsFailedItems :: Lens' AddAttributesToFindingsResponse (HashMap Text FailedItemDetails)
aatfrsFailedItems = lens _aatfrsFailedItems (\ s a -> s{_aatfrsFailedItems = a}) . _Map

instance NFData AddAttributesToFindingsResponse where
