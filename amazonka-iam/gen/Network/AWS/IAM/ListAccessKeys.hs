{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.IAM.ListAccessKeys
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Returns information about the access key IDs associated with the
-- specified user. If there are none, the action returns an empty list.
--
-- Although each user is limited to a small number of keys, you can still
-- paginate the results using the @MaxItems@ and @Marker@ parameters.
--
-- If the @UserName@ field is not specified, the UserName is determined
-- implicitly based on the AWS access key ID used to sign the request.
-- Because this action works for access keys under the AWS account, you can
-- use this action to manage root credentials even if the AWS account has
-- no associated users.
--
-- To ensure the security of your AWS account, the secret access key is
-- accessible only during key and user creation.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_ListAccessKeys.html>
module Network.AWS.IAM.ListAccessKeys
    (
    -- * Request
      ListAccessKeys
    -- ** Request constructor
    , listAccessKeys
    -- ** Request lenses
    , lakUserName
    , lakMaxItems
    , lakMarker

    -- * Response
    , ListAccessKeysResponse
    -- ** Response constructor
    , listAccessKeysResponse
    -- ** Response lenses
    , lakrMarker
    , lakrIsTruncated
    , lakrAccessKeyMetadata
    , lakrStatus
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'listAccessKeys' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lakUserName'
--
-- * 'lakMaxItems'
--
-- * 'lakMarker'
data ListAccessKeys = ListAccessKeys'
    { _lakUserName :: !(Maybe Text)
    , _lakMaxItems :: !(Maybe Nat)
    , _lakMarker   :: !(Maybe Text)
    } deriving (Eq,Read,Show)

-- | 'ListAccessKeys' smart constructor.
listAccessKeys :: ListAccessKeys
listAccessKeys =
    ListAccessKeys'
    { _lakUserName = Nothing
    , _lakMaxItems = Nothing
    , _lakMarker = Nothing
    }

-- | The name of the user.
lakUserName :: Lens' ListAccessKeys (Maybe Text)
lakUserName = lens _lakUserName (\ s a -> s{_lakUserName = a});

-- | Use this parameter only when paginating results to indicate the maximum
-- number of keys you want in the response. If there are additional keys
-- beyond the maximum you specify, the @IsTruncated@ response element is
-- @true@. This parameter is optional. If you do not include it, it
-- defaults to 100.
lakMaxItems :: Lens' ListAccessKeys (Maybe Natural)
lakMaxItems = lens _lakMaxItems (\ s a -> s{_lakMaxItems = a}) . mapping _Nat;

-- | Use this parameter only when paginating results, and only in a
-- subsequent request after you\'ve received a response where the results
-- are truncated. Set it to the value of the @Marker@ element in the
-- response you just received.
lakMarker :: Lens' ListAccessKeys (Maybe Text)
lakMarker = lens _lakMarker (\ s a -> s{_lakMarker = a});

instance AWSPager ListAccessKeys where
        page rq rs
          | stop (rs ^. lakrIsTruncated) = Nothing
          | isNothing (rs ^. lakrMarker) = Nothing
          | otherwise =
            Just $ rq & lakMarker .~ rs ^. lakrMarker

instance AWSRequest ListAccessKeys where
        type Sv ListAccessKeys = IAM
        type Rs ListAccessKeys = ListAccessKeysResponse
        request = post
        response
          = receiveXMLWrapper "ListAccessKeysResult"
              (\ s h x ->
                 ListAccessKeysResponse' <$>
                   (x .@? "Marker") <*> (x .@? "IsTruncated") <*>
                     (x .@? "AccessKeyMetadata" .!@ mempty >>=
                        parseXMLList "member")
                     <*> (pure s))

instance ToHeaders ListAccessKeys where
        toHeaders = const mempty

instance ToPath ListAccessKeys where
        toPath = const "/"

instance ToQuery ListAccessKeys where
        toQuery ListAccessKeys'{..}
          = mconcat
              ["Action" =: ("ListAccessKeys" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "UserName" =: _lakUserName,
               "MaxItems" =: _lakMaxItems, "Marker" =: _lakMarker]

-- | Contains the response to a successful ListAccessKeys request.
--
-- /See:/ 'listAccessKeysResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lakrMarker'
--
-- * 'lakrIsTruncated'
--
-- * 'lakrAccessKeyMetadata'
--
-- * 'lakrStatus'
data ListAccessKeysResponse = ListAccessKeysResponse'
    { _lakrMarker            :: !(Maybe Text)
    , _lakrIsTruncated       :: !(Maybe Bool)
    , _lakrAccessKeyMetadata :: ![AccessKeyMetadata]
    , _lakrStatus            :: !Status
    } deriving (Eq,Show)

-- | 'ListAccessKeysResponse' smart constructor.
listAccessKeysResponse :: Status -> ListAccessKeysResponse
listAccessKeysResponse pStatus =
    ListAccessKeysResponse'
    { _lakrMarker = Nothing
    , _lakrIsTruncated = Nothing
    , _lakrAccessKeyMetadata = mempty
    , _lakrStatus = pStatus
    }

-- | If @IsTruncated@ is @true@, this element is present and contains the
-- value to use for the @Marker@ parameter in a subsequent pagination
-- request.
lakrMarker :: Lens' ListAccessKeysResponse (Maybe Text)
lakrMarker = lens _lakrMarker (\ s a -> s{_lakrMarker = a});

-- | A flag that indicates whether there are more keys to list. If your
-- results were truncated, you can make a subsequent pagination request
-- using the @Marker@ request parameter to retrieve more keys in the list.
lakrIsTruncated :: Lens' ListAccessKeysResponse (Maybe Bool)
lakrIsTruncated = lens _lakrIsTruncated (\ s a -> s{_lakrIsTruncated = a});

-- | A list of access key metadata.
lakrAccessKeyMetadata :: Lens' ListAccessKeysResponse [AccessKeyMetadata]
lakrAccessKeyMetadata = lens _lakrAccessKeyMetadata (\ s a -> s{_lakrAccessKeyMetadata = a});

-- | FIXME: Undocumented member.
lakrStatus :: Lens' ListAccessKeysResponse Status
lakrStatus = lens _lakrStatus (\ s a -> s{_lakrStatus = a});
