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
-- Module      : Network.AWS.IAM.ListAccessKeys
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the access key IDs associated with the specified IAM user. If there are none, the operation returns an empty list.
--
--
-- Although each user is limited to a small number of keys, you can still paginate the results using the @MaxItems@ and @Marker@ parameters.
--
-- If the @UserName@ field is not specified, the user name is determined implicitly based on the AWS access key ID used to sign the request. Because this operation works for access keys under the AWS account, you can use this operation to manage AWS account root user credentials even if the AWS account has no associated users.
--
--
-- This operation returns paginated results.
module Network.AWS.IAM.ListAccessKeys
    (
    -- * Creating a Request
      listAccessKeys
    , ListAccessKeys
    -- * Request Lenses
    , lakUserName
    , lakMarker
    , lakMaxItems

    -- * Destructuring the Response
    , listAccessKeysResponse
    , ListAccessKeysResponse
    -- * Response Lenses
    , lakrsMarker
    , lakrsIsTruncated
    , lakrsResponseStatus
    , lakrsAccessKeyMetadata
    ) where

import Network.AWS.IAM.Types
import Network.AWS.IAM.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listAccessKeys' smart constructor.
data ListAccessKeys = ListAccessKeys'
  { _lakUserName :: !(Maybe Text)
  , _lakMarker   :: !(Maybe Text)
  , _lakMaxItems :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListAccessKeys' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lakUserName' - The name of the user. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- * 'lakMarker' - Use this parameter only when paginating results and only after you receive a response indicating that the results are truncated. Set it to the value of the @Marker@ element in the response that you received to indicate where the next call should start.
--
-- * 'lakMaxItems' - (Optional) Use this only when paginating results to indicate the maximum number of items you want in the response. If additional items exist beyond the maximum you specify, the @IsTruncated@ response element is @true@ . If you do not include this parameter, it defaults to 100. Note that IAM might return fewer results, even when there are more results available. In that case, the @IsTruncated@ response element returns @true@ and @Marker@ contains a value to include in the subsequent call that tells the service where to continue from.
listAccessKeys
    :: ListAccessKeys
listAccessKeys =
  ListAccessKeys'
    {_lakUserName = Nothing, _lakMarker = Nothing, _lakMaxItems = Nothing}


-- | The name of the user. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
lakUserName :: Lens' ListAccessKeys (Maybe Text)
lakUserName = lens _lakUserName (\ s a -> s{_lakUserName = a})

-- | Use this parameter only when paginating results and only after you receive a response indicating that the results are truncated. Set it to the value of the @Marker@ element in the response that you received to indicate where the next call should start.
lakMarker :: Lens' ListAccessKeys (Maybe Text)
lakMarker = lens _lakMarker (\ s a -> s{_lakMarker = a})

-- | (Optional) Use this only when paginating results to indicate the maximum number of items you want in the response. If additional items exist beyond the maximum you specify, the @IsTruncated@ response element is @true@ . If you do not include this parameter, it defaults to 100. Note that IAM might return fewer results, even when there are more results available. In that case, the @IsTruncated@ response element returns @true@ and @Marker@ contains a value to include in the subsequent call that tells the service where to continue from.
lakMaxItems :: Lens' ListAccessKeys (Maybe Natural)
lakMaxItems = lens _lakMaxItems (\ s a -> s{_lakMaxItems = a}) . mapping _Nat

instance AWSPager ListAccessKeys where
        page rq rs
          | stop (rs ^. lakrsIsTruncated) = Nothing
          | isNothing (rs ^. lakrsMarker) = Nothing
          | otherwise =
            Just $ rq & lakMarker .~ rs ^. lakrsMarker

instance AWSRequest ListAccessKeys where
        type Rs ListAccessKeys = ListAccessKeysResponse
        request = postQuery iam
        response
          = receiveXMLWrapper "ListAccessKeysResult"
              (\ s h x ->
                 ListAccessKeysResponse' <$>
                   (x .@? "Marker") <*> (x .@? "IsTruncated") <*>
                     (pure (fromEnum s))
                     <*>
                     (x .@? "AccessKeyMetadata" .!@ mempty >>=
                        parseXMLList "member"))

instance Hashable ListAccessKeys where

instance NFData ListAccessKeys where

instance ToHeaders ListAccessKeys where
        toHeaders = const mempty

instance ToPath ListAccessKeys where
        toPath = const "/"

instance ToQuery ListAccessKeys where
        toQuery ListAccessKeys'{..}
          = mconcat
              ["Action" =: ("ListAccessKeys" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "UserName" =: _lakUserName, "Marker" =: _lakMarker,
               "MaxItems" =: _lakMaxItems]

-- | Contains the response to a successful 'ListAccessKeys' request.
--
--
--
-- /See:/ 'listAccessKeysResponse' smart constructor.
data ListAccessKeysResponse = ListAccessKeysResponse'
  { _lakrsMarker            :: !(Maybe Text)
  , _lakrsIsTruncated       :: !(Maybe Bool)
  , _lakrsResponseStatus    :: !Int
  , _lakrsAccessKeyMetadata :: ![AccessKeyMetadata]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListAccessKeysResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lakrsMarker' - When @IsTruncated@ is @true@ , this element is present and contains the value to use for the @Marker@ parameter in a subsequent pagination request.
--
-- * 'lakrsIsTruncated' - A flag that indicates whether there are more items to return. If your results were truncated, you can make a subsequent pagination request using the @Marker@ request parameter to retrieve more items. Note that IAM might return fewer than the @MaxItems@ number of results even when there are more results available. We recommend that you check @IsTruncated@ after every call to ensure that you receive all of your results.
--
-- * 'lakrsResponseStatus' - -- | The response status code.
--
-- * 'lakrsAccessKeyMetadata' - A list of objects containing metadata about the access keys.
listAccessKeysResponse
    :: Int -- ^ 'lakrsResponseStatus'
    -> ListAccessKeysResponse
listAccessKeysResponse pResponseStatus_ =
  ListAccessKeysResponse'
    { _lakrsMarker = Nothing
    , _lakrsIsTruncated = Nothing
    , _lakrsResponseStatus = pResponseStatus_
    , _lakrsAccessKeyMetadata = mempty
    }


-- | When @IsTruncated@ is @true@ , this element is present and contains the value to use for the @Marker@ parameter in a subsequent pagination request.
lakrsMarker :: Lens' ListAccessKeysResponse (Maybe Text)
lakrsMarker = lens _lakrsMarker (\ s a -> s{_lakrsMarker = a})

-- | A flag that indicates whether there are more items to return. If your results were truncated, you can make a subsequent pagination request using the @Marker@ request parameter to retrieve more items. Note that IAM might return fewer than the @MaxItems@ number of results even when there are more results available. We recommend that you check @IsTruncated@ after every call to ensure that you receive all of your results.
lakrsIsTruncated :: Lens' ListAccessKeysResponse (Maybe Bool)
lakrsIsTruncated = lens _lakrsIsTruncated (\ s a -> s{_lakrsIsTruncated = a})

-- | -- | The response status code.
lakrsResponseStatus :: Lens' ListAccessKeysResponse Int
lakrsResponseStatus = lens _lakrsResponseStatus (\ s a -> s{_lakrsResponseStatus = a})

-- | A list of objects containing metadata about the access keys.
lakrsAccessKeyMetadata :: Lens' ListAccessKeysResponse [AccessKeyMetadata]
lakrsAccessKeyMetadata = lens _lakrsAccessKeyMetadata (\ s a -> s{_lakrsAccessKeyMetadata = a}) . _Coerce

instance NFData ListAccessKeysResponse where
