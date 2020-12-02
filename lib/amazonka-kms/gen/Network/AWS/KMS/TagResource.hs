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
-- Module      : Network.AWS.KMS.TagResource
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds or overwrites one or more tags for the specified customer master key (CMK). You cannot perform this operation on a CMK in a different AWS account.
--
--
-- Each tag consists of a tag key and a tag value. Tag keys and tag values are both required, but tag values can be empty (null) strings.
--
-- You cannot use the same tag key more than once per CMK. For example, consider a CMK with one tag whose tag key is @Purpose@ and tag value is @Test@ . If you send a @TagResource@ request for this CMK with a tag key of @Purpose@ and a tag value of @Prod@ , it does not create a second tag. Instead, the original tag is overwritten with the new tag value.
--
-- For information about the rules that apply to tag keys and tag values, see <http://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/allocation-tag-restrictions.html User-Defined Tag Restrictions> in the /AWS Billing and Cost Management User Guide/ .
--
module Network.AWS.KMS.TagResource
    (
    -- * Creating a Request
      tagResource
    , TagResource
    -- * Request Lenses
    , trKeyId
    , trTags

    -- * Destructuring the Response
    , tagResourceResponse
    , TagResourceResponse
    ) where

import Network.AWS.KMS.Types
import Network.AWS.KMS.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'tagResource' smart constructor.
data TagResource = TagResource'
  { _trKeyId :: !Text
  , _trTags  :: ![Tag]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TagResource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'trKeyId' - A unique identifier for the CMK you are tagging. Specify the key ID or the Amazon Resource Name (ARN) of the CMK. For example:     * Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@      * Key ARN: @arn:aws:kms:us-east-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab@  To get the key ID and key ARN for a CMK, use 'ListKeys' or 'DescribeKey' .
--
-- * 'trTags' - One or more tags. Each tag consists of a tag key and a tag value.
tagResource
    :: Text -- ^ 'trKeyId'
    -> TagResource
tagResource pKeyId_ = TagResource' {_trKeyId = pKeyId_, _trTags = mempty}


-- | A unique identifier for the CMK you are tagging. Specify the key ID or the Amazon Resource Name (ARN) of the CMK. For example:     * Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@      * Key ARN: @arn:aws:kms:us-east-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab@  To get the key ID and key ARN for a CMK, use 'ListKeys' or 'DescribeKey' .
trKeyId :: Lens' TagResource Text
trKeyId = lens _trKeyId (\ s a -> s{_trKeyId = a})

-- | One or more tags. Each tag consists of a tag key and a tag value.
trTags :: Lens' TagResource [Tag]
trTags = lens _trTags (\ s a -> s{_trTags = a}) . _Coerce

instance AWSRequest TagResource where
        type Rs TagResource = TagResourceResponse
        request = postJSON kms
        response = receiveNull TagResourceResponse'

instance Hashable TagResource where

instance NFData TagResource where

instance ToHeaders TagResource where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("TrentService.TagResource" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON TagResource where
        toJSON TagResource'{..}
          = object
              (catMaybes
                 [Just ("KeyId" .= _trKeyId),
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
