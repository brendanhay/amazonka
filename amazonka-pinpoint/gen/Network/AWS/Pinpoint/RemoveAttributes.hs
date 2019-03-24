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
-- Module      : Network.AWS.Pinpoint.RemoveAttributes
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Used to remove the attributes for an app
module Network.AWS.Pinpoint.RemoveAttributes
    (
    -- * Creating a Request
      removeAttributes
    , RemoveAttributes
    -- * Request Lenses
    , raAttributeType
    , raApplicationId
    , raUpdateAttributesRequest

    -- * Destructuring the Response
    , removeAttributesResponse
    , RemoveAttributesResponse
    -- * Response Lenses
    , rarsResponseStatus
    , rarsAttributesResource
    ) where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types
import Network.AWS.Pinpoint.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'removeAttributes' smart constructor.
data RemoveAttributes = RemoveAttributes'
  { _raAttributeType           :: !Text
  , _raApplicationId           :: !Text
  , _raUpdateAttributesRequest :: !UpdateAttributesRequest
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RemoveAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'raAttributeType' - Type of attribute. Can be endpoint-custom-attributes, endpoint-custom-metrics, endpoint-user-attributes.
--
-- * 'raApplicationId' - The unique ID of your Amazon Pinpoint application.
--
-- * 'raUpdateAttributesRequest' - Undocumented member.
removeAttributes
    :: Text -- ^ 'raAttributeType'
    -> Text -- ^ 'raApplicationId'
    -> UpdateAttributesRequest -- ^ 'raUpdateAttributesRequest'
    -> RemoveAttributes
removeAttributes pAttributeType_ pApplicationId_ pUpdateAttributesRequest_ =
  RemoveAttributes'
    { _raAttributeType = pAttributeType_
    , _raApplicationId = pApplicationId_
    , _raUpdateAttributesRequest = pUpdateAttributesRequest_
    }


-- | Type of attribute. Can be endpoint-custom-attributes, endpoint-custom-metrics, endpoint-user-attributes.
raAttributeType :: Lens' RemoveAttributes Text
raAttributeType = lens _raAttributeType (\ s a -> s{_raAttributeType = a})

-- | The unique ID of your Amazon Pinpoint application.
raApplicationId :: Lens' RemoveAttributes Text
raApplicationId = lens _raApplicationId (\ s a -> s{_raApplicationId = a})

-- | Undocumented member.
raUpdateAttributesRequest :: Lens' RemoveAttributes UpdateAttributesRequest
raUpdateAttributesRequest = lens _raUpdateAttributesRequest (\ s a -> s{_raUpdateAttributesRequest = a})

instance AWSRequest RemoveAttributes where
        type Rs RemoveAttributes = RemoveAttributesResponse
        request = putJSON pinpoint
        response
          = receiveJSON
              (\ s h x ->
                 RemoveAttributesResponse' <$>
                   (pure (fromEnum s)) <*> (eitherParseJSON x))

instance Hashable RemoveAttributes where

instance NFData RemoveAttributes where

instance ToHeaders RemoveAttributes where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON RemoveAttributes where
        toJSON RemoveAttributes'{..}
          = object
              (catMaybes
                 [Just
                    ("UpdateAttributesRequest" .=
                       _raUpdateAttributesRequest)])

instance ToPath RemoveAttributes where
        toPath RemoveAttributes'{..}
          = mconcat
              ["/v1/apps/", toBS _raApplicationId, "/attributes/",
               toBS _raAttributeType]

instance ToQuery RemoveAttributes where
        toQuery = const mempty

-- | /See:/ 'removeAttributesResponse' smart constructor.
data RemoveAttributesResponse = RemoveAttributesResponse'
  { _rarsResponseStatus     :: !Int
  , _rarsAttributesResource :: !AttributesResource
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RemoveAttributesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rarsResponseStatus' - -- | The response status code.
--
-- * 'rarsAttributesResource' - Undocumented member.
removeAttributesResponse
    :: Int -- ^ 'rarsResponseStatus'
    -> AttributesResource -- ^ 'rarsAttributesResource'
    -> RemoveAttributesResponse
removeAttributesResponse pResponseStatus_ pAttributesResource_ =
  RemoveAttributesResponse'
    { _rarsResponseStatus = pResponseStatus_
    , _rarsAttributesResource = pAttributesResource_
    }


-- | -- | The response status code.
rarsResponseStatus :: Lens' RemoveAttributesResponse Int
rarsResponseStatus = lens _rarsResponseStatus (\ s a -> s{_rarsResponseStatus = a})

-- | Undocumented member.
rarsAttributesResource :: Lens' RemoveAttributesResponse AttributesResource
rarsAttributesResource = lens _rarsAttributesResource (\ s a -> s{_rarsAttributesResource = a})

instance NFData RemoveAttributesResponse where
