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
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes entire attributes (key and value pairs) from the findings that are specified by the ARNs of the findings where an attribute with the specified key exists.
--
--
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
    , raffrsResponseStatus
    , raffrsFailedItems
    ) where

import Network.AWS.Inspector.Types
import Network.AWS.Inspector.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'removeAttributesFromFindings' smart constructor.
data RemoveAttributesFromFindings = RemoveAttributesFromFindings'
  { _raffFindingARNs   :: !(List1 Text)
  , _raffAttributeKeys :: ![Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RemoveAttributesFromFindings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'raffFindingARNs' - The ARNs that specify the findings that you want to remove attributes from.
--
-- * 'raffAttributeKeys' - The array of attribute keys that you want to remove from specified findings.
removeAttributesFromFindings
    :: NonEmpty Text -- ^ 'raffFindingARNs'
    -> RemoveAttributesFromFindings
removeAttributesFromFindings pFindingARNs_ =
  RemoveAttributesFromFindings'
    {_raffFindingARNs = _List1 # pFindingARNs_, _raffAttributeKeys = mempty}


-- | The ARNs that specify the findings that you want to remove attributes from.
raffFindingARNs :: Lens' RemoveAttributesFromFindings (NonEmpty Text)
raffFindingARNs = lens _raffFindingARNs (\ s a -> s{_raffFindingARNs = a}) . _List1

-- | The array of attribute keys that you want to remove from specified findings.
raffAttributeKeys :: Lens' RemoveAttributesFromFindings [Text]
raffAttributeKeys = lens _raffAttributeKeys (\ s a -> s{_raffAttributeKeys = a}) . _Coerce

instance AWSRequest RemoveAttributesFromFindings
         where
        type Rs RemoveAttributesFromFindings =
             RemoveAttributesFromFindingsResponse
        request = postJSON inspector
        response
          = receiveJSON
              (\ s h x ->
                 RemoveAttributesFromFindingsResponse' <$>
                   (pure (fromEnum s)) <*>
                     (x .?> "failedItems" .!@ mempty))

instance Hashable RemoveAttributesFromFindings where

instance NFData RemoveAttributesFromFindings where

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
                 [Just ("findingArns" .= _raffFindingARNs),
                  Just ("attributeKeys" .= _raffAttributeKeys)])

instance ToPath RemoveAttributesFromFindings where
        toPath = const "/"

instance ToQuery RemoveAttributesFromFindings where
        toQuery = const mempty

-- | /See:/ 'removeAttributesFromFindingsResponse' smart constructor.
data RemoveAttributesFromFindingsResponse = RemoveAttributesFromFindingsResponse'
  { _raffrsResponseStatus :: !Int
  , _raffrsFailedItems    :: !(Map Text FailedItemDetails)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RemoveAttributesFromFindingsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'raffrsResponseStatus' - -- | The response status code.
--
-- * 'raffrsFailedItems' - Attributes details that cannot be described. An error code is provided for each failed item.
removeAttributesFromFindingsResponse
    :: Int -- ^ 'raffrsResponseStatus'
    -> RemoveAttributesFromFindingsResponse
removeAttributesFromFindingsResponse pResponseStatus_ =
  RemoveAttributesFromFindingsResponse'
    {_raffrsResponseStatus = pResponseStatus_, _raffrsFailedItems = mempty}


-- | -- | The response status code.
raffrsResponseStatus :: Lens' RemoveAttributesFromFindingsResponse Int
raffrsResponseStatus = lens _raffrsResponseStatus (\ s a -> s{_raffrsResponseStatus = a})

-- | Attributes details that cannot be described. An error code is provided for each failed item.
raffrsFailedItems :: Lens' RemoveAttributesFromFindingsResponse (HashMap Text FailedItemDetails)
raffrsFailedItems = lens _raffrsFailedItems (\ s a -> s{_raffrsFailedItems = a}) . _Map

instance NFData RemoveAttributesFromFindingsResponse
         where
