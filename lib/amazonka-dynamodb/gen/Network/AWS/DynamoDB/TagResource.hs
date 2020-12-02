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
-- Module      : Network.AWS.DynamoDB.TagResource
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associate a set of tags with an Amazon DynamoDB resource. You can then activate these user-defined tags so that they appear on the Billing and Cost Management console for cost allocation tracking. You can call TagResource up to 5 times per second, per account.
--
--
-- For an overview on tagging DynamoDB resources, see <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Tagging.html Tagging for DynamoDB> in the /Amazon DynamoDB Developer Guide/ .
--
module Network.AWS.DynamoDB.TagResource
    (
    -- * Creating a Request
      tagResource
    , TagResource
    -- * Request Lenses
    , trResourceARN
    , trTags

    -- * Destructuring the Response
    , tagResourceResponse
    , TagResourceResponse
    ) where

import Network.AWS.DynamoDB.Types
import Network.AWS.DynamoDB.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'tagResource' smart constructor.
data TagResource = TagResource'
  { _trResourceARN :: !Text
  , _trTags        :: ![Tag]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TagResource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'trResourceARN' - Identifies the Amazon DynamoDB resource to which tags should be added. This value is an Amazon Resource Name (ARN).
--
-- * 'trTags' - The tags to be assigned to the Amazon DynamoDB resource.
tagResource
    :: Text -- ^ 'trResourceARN'
    -> TagResource
tagResource pResourceARN_ =
  TagResource' {_trResourceARN = pResourceARN_, _trTags = mempty}


-- | Identifies the Amazon DynamoDB resource to which tags should be added. This value is an Amazon Resource Name (ARN).
trResourceARN :: Lens' TagResource Text
trResourceARN = lens _trResourceARN (\ s a -> s{_trResourceARN = a})

-- | The tags to be assigned to the Amazon DynamoDB resource.
trTags :: Lens' TagResource [Tag]
trTags = lens _trTags (\ s a -> s{_trTags = a}) . _Coerce

instance AWSRequest TagResource where
        type Rs TagResource = TagResourceResponse
        request = postJSON dynamoDB
        response = receiveNull TagResourceResponse'

instance Hashable TagResource where

instance NFData TagResource where

instance ToHeaders TagResource where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DynamoDB_20120810.TagResource" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.0" :: ByteString)])

instance ToJSON TagResource where
        toJSON TagResource'{..}
          = object
              (catMaybes
                 [Just ("ResourceArn" .= _trResourceARN),
                  Just ("Tags" .= _trTags)])

instance ToPath TagResource where
        toPath = const "/"

instance ToQuery TagResource where
        toQuery = const mempty

-- | /See:/ 'tagResourceResponse' smart constructor.
data TagResourceResponse =
  TagResourceResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TagResourceResponse' with the minimum fields required to make a request.
--
tagResourceResponse
    :: TagResourceResponse
tagResourceResponse = TagResourceResponse'


instance NFData TagResourceResponse where
