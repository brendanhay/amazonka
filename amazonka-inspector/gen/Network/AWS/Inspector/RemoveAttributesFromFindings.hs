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
-- Module      : Network.AWS.Inspector.RemoveAttributesFromFindings
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the entire attribute (key and value pair) from the findings
-- specified by the finding ARNs where an attribute with the specified key
-- exists.
--
-- /See:/ <http://docs.aws.amazon.com/inspector/latest/APIReference/API_RemoveAttributesFromFindings.html AWS API Reference> for RemoveAttributesFromFindings.
module Network.AWS.Inspector.RemoveAttributesFromFindings
    (
    -- * Creating a Request
      removeAttributesFromFindings
    , RemoveAttributesFromFindings
    -- * Request Lenses
    , raffFindingARNs
    , raffAttributeKeys

    -- * Destructuring the Response
    , removeAttributesFromFindingsResponse
    , RemoveAttributesFromFindingsResponse
    -- * Response Lenses
    , raffrsMessage
    , raffrsResponseStatus
    ) where

import           Network.AWS.Inspector.Types
import           Network.AWS.Inspector.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'removeAttributesFromFindings' smart constructor.
data RemoveAttributesFromFindings = RemoveAttributesFromFindings'
    { _raffFindingARNs   :: !(Maybe [Text])
    , _raffAttributeKeys :: !(Maybe [Text])
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'RemoveAttributesFromFindings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'raffFindingARNs'
--
-- * 'raffAttributeKeys'
removeAttributesFromFindings
    :: RemoveAttributesFromFindings
removeAttributesFromFindings =
    RemoveAttributesFromFindings'
    { _raffFindingARNs = Nothing
    , _raffAttributeKeys = Nothing
    }

-- | The ARNs specifying the findings that you want to remove attributes
-- from.
raffFindingARNs :: Lens' RemoveAttributesFromFindings [Text]
raffFindingARNs = lens _raffFindingARNs (\ s a -> s{_raffFindingARNs = a}) . _Default . _Coerce;

-- | The array of attribute keys that you want to remove from specified
-- findings.
raffAttributeKeys :: Lens' RemoveAttributesFromFindings [Text]
raffAttributeKeys = lens _raffAttributeKeys (\ s a -> s{_raffAttributeKeys = a}) . _Default . _Coerce;

instance AWSRequest RemoveAttributesFromFindings
         where
        type Rs RemoveAttributesFromFindings =
             RemoveAttributesFromFindingsResponse
        request = postJSON inspector
        response
          = receiveJSON
              (\ s h x ->
                 RemoveAttributesFromFindingsResponse' <$>
                   (x .?> "message") <*> (pure (fromEnum s)))

instance ToHeaders RemoveAttributesFromFindings where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("InspectorService.RemoveAttributesFromFindings" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON RemoveAttributesFromFindings where
        toJSON RemoveAttributesFromFindings'{..}
          = object
              (catMaybes
                 [("findingArns" .=) <$> _raffFindingARNs,
                  ("attributeKeys" .=) <$> _raffAttributeKeys])

instance ToPath RemoveAttributesFromFindings where
        toPath = const "/"

instance ToQuery RemoveAttributesFromFindings where
        toQuery = const mempty

-- | /See:/ 'removeAttributesFromFindingsResponse' smart constructor.
data RemoveAttributesFromFindingsResponse = RemoveAttributesFromFindingsResponse'
    { _raffrsMessage        :: !(Maybe Text)
    , _raffrsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'RemoveAttributesFromFindingsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'raffrsMessage'
--
-- * 'raffrsResponseStatus'
removeAttributesFromFindingsResponse
    :: Int -- ^ 'raffrsResponseStatus'
    -> RemoveAttributesFromFindingsResponse
removeAttributesFromFindingsResponse pResponseStatus_ =
    RemoveAttributesFromFindingsResponse'
    { _raffrsMessage = Nothing
    , _raffrsResponseStatus = pResponseStatus_
    }

-- | Confirmation details of the action performed.
raffrsMessage :: Lens' RemoveAttributesFromFindingsResponse (Maybe Text)
raffrsMessage = lens _raffrsMessage (\ s a -> s{_raffrsMessage = a});

-- | The response status code.
raffrsResponseStatus :: Lens' RemoveAttributesFromFindingsResponse Int
raffrsResponseStatus = lens _raffrsResponseStatus (\ s a -> s{_raffrsResponseStatus = a});
