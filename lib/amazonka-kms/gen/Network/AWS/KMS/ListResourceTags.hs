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
-- Module      : Network.AWS.KMS.ListResourceTags
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of all tags for the specified customer master key (CMK).
--
--
-- You cannot perform this operation on a CMK in a different AWS account.
--
module Network.AWS.KMS.ListResourceTags
    (
    -- * Creating a Request
      listResourceTags
    , ListResourceTags
    -- * Request Lenses
    , lrtMarker
    , lrtLimit
    , lrtKeyId

    -- * Destructuring the Response
    , listResourceTagsResponse
    , ListResourceTagsResponse
    -- * Response Lenses
    , lrtrsTruncated
    , lrtrsNextMarker
    , lrtrsTags
    , lrtrsResponseStatus
    ) where

import Network.AWS.KMS.Types
import Network.AWS.KMS.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listResourceTags' smart constructor.
data ListResourceTags = ListResourceTags'
  { _lrtMarker :: !(Maybe Text)
  , _lrtLimit  :: !(Maybe Nat)
  , _lrtKeyId  :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListResourceTags' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lrtMarker' - Use this parameter in a subsequent request after you receive a response with truncated results. Set it to the value of @NextMarker@ from the truncated response you just received. Do not attempt to construct this value. Use only the value of @NextMarker@ from the truncated response you just received.
--
-- * 'lrtLimit' - Use this parameter to specify the maximum number of items to return. When this value is present, AWS KMS does not return more than the specified number of items, but it might return fewer. This value is optional. If you include a value, it must be between 1 and 50, inclusive. If you do not include a value, it defaults to 50.
--
-- * 'lrtKeyId' - A unique identifier for the customer master key (CMK). Specify the key ID or the Amazon Resource Name (ARN) of the CMK. For example:     * Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@      * Key ARN: @arn:aws:kms:us-east-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab@  To get the key ID and key ARN for a CMK, use 'ListKeys' or 'DescribeKey' .
listResourceTags
    :: Text -- ^ 'lrtKeyId'
    -> ListResourceTags
listResourceTags pKeyId_ =
  ListResourceTags'
    {_lrtMarker = Nothing, _lrtLimit = Nothing, _lrtKeyId = pKeyId_}


-- | Use this parameter in a subsequent request after you receive a response with truncated results. Set it to the value of @NextMarker@ from the truncated response you just received. Do not attempt to construct this value. Use only the value of @NextMarker@ from the truncated response you just received.
lrtMarker :: Lens' ListResourceTags (Maybe Text)
lrtMarker = lens _lrtMarker (\ s a -> s{_lrtMarker = a})

-- | Use this parameter to specify the maximum number of items to return. When this value is present, AWS KMS does not return more than the specified number of items, but it might return fewer. This value is optional. If you include a value, it must be between 1 and 50, inclusive. If you do not include a value, it defaults to 50.
lrtLimit :: Lens' ListResourceTags (Maybe Natural)
lrtLimit = lens _lrtLimit (\ s a -> s{_lrtLimit = a}) . mapping _Nat

-- | A unique identifier for the customer master key (CMK). Specify the key ID or the Amazon Resource Name (ARN) of the CMK. For example:     * Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@      * Key ARN: @arn:aws:kms:us-east-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab@  To get the key ID and key ARN for a CMK, use 'ListKeys' or 'DescribeKey' .
lrtKeyId :: Lens' ListResourceTags Text
lrtKeyId = lens _lrtKeyId (\ s a -> s{_lrtKeyId = a})

instance AWSRequest ListResourceTags where
        type Rs ListResourceTags = ListResourceTagsResponse
        request = postJSON kms
        response
          = receiveJSON
              (\ s h x ->
                 ListResourceTagsResponse' <$>
                   (x .?> "Truncated") <*> (x .?> "NextMarker") <*>
                     (x .?> "Tags" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable ListResourceTags where

instance NFData ListResourceTags where

instance ToHeaders ListResourceTags where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("TrentService.ListResourceTags" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListResourceTags where
        toJSON ListResourceTags'{..}
          = object
              (catMaybes
                 [("Marker" .=) <$> _lrtMarker,
                  ("Limit" .=) <$> _lrtLimit,
                  Just ("KeyId" .= _lrtKeyId)])

instance ToPath ListResourceTags where
        toPath = const "/"

instance ToQuery ListResourceTags where
        toQuery = const mempty

-- | /See:/ 'listResourceTagsResponse' smart constructor.
data ListResourceTagsResponse = ListResourceTagsResponse'
  { _lrtrsTruncated      :: !(Maybe Bool)
  , _lrtrsNextMarker     :: !(Maybe Text)
  , _lrtrsTags           :: !(Maybe [Tag])
  , _lrtrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListResourceTagsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lrtrsTruncated' - A flag that indicates whether there are more items in the list. When this value is true, the list in this response is truncated. To get more items, pass the value of the @NextMarker@ element in this response to the @Marker@ parameter in a subsequent request.
--
-- * 'lrtrsNextMarker' - When @Truncated@ is true, this element is present and contains the value to use for the @Marker@ parameter in a subsequent request. Do not assume or infer any information from this value.
--
-- * 'lrtrsTags' - A list of tags. Each tag consists of a tag key and a tag value.
--
-- * 'lrtrsResponseStatus' - -- | The response status code.
listResourceTagsResponse
    :: Int -- ^ 'lrtrsResponseStatus'
    -> ListResourceTagsResponse
listResourceTagsResponse pResponseStatus_ =
  ListResourceTagsResponse'
    { _lrtrsTruncated = Nothing
    , _lrtrsNextMarker = Nothing
    , _lrtrsTags = Nothing
    , _lrtrsResponseStatus = pResponseStatus_
    }


-- | A flag that indicates whether there are more items in the list. When this value is true, the list in this response is truncated. To get more items, pass the value of the @NextMarker@ element in this response to the @Marker@ parameter in a subsequent request.
lrtrsTruncated :: Lens' ListResourceTagsResponse (Maybe Bool)
lrtrsTruncated = lens _lrtrsTruncated (\ s a -> s{_lrtrsTruncated = a})

-- | When @Truncated@ is true, this element is present and contains the value to use for the @Marker@ parameter in a subsequent request. Do not assume or infer any information from this value.
lrtrsNextMarker :: Lens' ListResourceTagsResponse (Maybe Text)
lrtrsNextMarker = lens _lrtrsNextMarker (\ s a -> s{_lrtrsNextMarker = a})

-- | A list of tags. Each tag consists of a tag key and a tag value.
lrtrsTags :: Lens' ListResourceTagsResponse [Tag]
lrtrsTags = lens _lrtrsTags (\ s a -> s{_lrtrsTags = a}) . _Default . _Coerce

-- | -- | The response status code.
lrtrsResponseStatus :: Lens' ListResourceTagsResponse Int
lrtrsResponseStatus = lens _lrtrsResponseStatus (\ s a -> s{_lrtrsResponseStatus = a})

instance NFData ListResourceTagsResponse where
