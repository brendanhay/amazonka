{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.ListAccessKeys
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the access key IDs associated with the
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
    , lakrqUserName
    , lakrqMaxItems
    , lakrqMarker

    -- * Response
    , ListAccessKeysResponse
    -- ** Response constructor
    , listAccessKeysResponse
    -- ** Response lenses
    , lakrsMarker
    , lakrsIsTruncated
    , lakrsStatus
    , lakrsAccessKeyMetadata
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
-- * 'lakrqUserName'
--
-- * 'lakrqMaxItems'
--
-- * 'lakrqMarker'
data ListAccessKeys = ListAccessKeys'
    { _lakrqUserName :: !(Maybe Text)
    , _lakrqMaxItems :: !(Maybe Nat)
    , _lakrqMarker   :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListAccessKeys' smart constructor.
listAccessKeys :: ListAccessKeys
listAccessKeys =
    ListAccessKeys'
    { _lakrqUserName = Nothing
    , _lakrqMaxItems = Nothing
    , _lakrqMarker = Nothing
    }

-- | The name of the user.
lakrqUserName :: Lens' ListAccessKeys (Maybe Text)
lakrqUserName = lens _lakrqUserName (\ s a -> s{_lakrqUserName = a});

-- | Use this only when paginating results to indicate the maximum number of
-- items you want in the response. If there are additional items beyond the
-- maximum you specify, the @IsTruncated@ response element is @true@.
--
-- This parameter is optional. If you do not include it, it defaults to
-- 100.
lakrqMaxItems :: Lens' ListAccessKeys (Maybe Natural)
lakrqMaxItems = lens _lakrqMaxItems (\ s a -> s{_lakrqMaxItems = a}) . mapping _Nat;

-- | Use this parameter only when paginating results and only after you have
-- received a response where the results are truncated. Set it to the value
-- of the @Marker@ element in the response you just received.
lakrqMarker :: Lens' ListAccessKeys (Maybe Text)
lakrqMarker = lens _lakrqMarker (\ s a -> s{_lakrqMarker = a});

instance AWSPager ListAccessKeys where
        page rq rs
          | stop (rs ^. lakrsIsTruncated) = Nothing
          | isNothing (rs ^. lakrsMarker) = Nothing
          | otherwise =
            Just $ rq & lakrqMarker .~ rs ^. lakrsMarker

instance AWSRequest ListAccessKeys where
        type Sv ListAccessKeys = IAM
        type Rs ListAccessKeys = ListAccessKeysResponse
        request = post
        response
          = receiveXMLWrapper "ListAccessKeysResult"
              (\ s h x ->
                 ListAccessKeysResponse' <$>
                   (x .@? "Marker") <*> (x .@? "IsTruncated") <*>
                     (pure (fromEnum s))
                     <*>
                     (x .@? "AccessKeyMetadata" .!@ mempty >>=
                        parseXMLList "member"))

instance ToHeaders ListAccessKeys where
        toHeaders = const mempty

instance ToPath ListAccessKeys where
        toPath = const "/"

instance ToQuery ListAccessKeys where
        toQuery ListAccessKeys'{..}
          = mconcat
              ["Action" =: ("ListAccessKeys" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "UserName" =: _lakrqUserName,
               "MaxItems" =: _lakrqMaxItems,
               "Marker" =: _lakrqMarker]

-- | Contains the response to a successful ListAccessKeys request.
--
-- /See:/ 'listAccessKeysResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lakrsMarker'
--
-- * 'lakrsIsTruncated'
--
-- * 'lakrsStatus'
--
-- * 'lakrsAccessKeyMetadata'
data ListAccessKeysResponse = ListAccessKeysResponse'
    { _lakrsMarker            :: !(Maybe Text)
    , _lakrsIsTruncated       :: !(Maybe Bool)
    , _lakrsStatus            :: !Int
    , _lakrsAccessKeyMetadata :: ![AccessKeyMetadata]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListAccessKeysResponse' smart constructor.
listAccessKeysResponse :: Int -> ListAccessKeysResponse
listAccessKeysResponse pStatus_ =
    ListAccessKeysResponse'
    { _lakrsMarker = Nothing
    , _lakrsIsTruncated = Nothing
    , _lakrsStatus = pStatus_
    , _lakrsAccessKeyMetadata = mempty
    }

-- | When @IsTruncated@ is @true@, this element is present and contains the
-- value to use for the @Marker@ parameter in a subsequent pagination
-- request.
lakrsMarker :: Lens' ListAccessKeysResponse (Maybe Text)
lakrsMarker = lens _lakrsMarker (\ s a -> s{_lakrsMarker = a});

-- | A flag that indicates whether there are more items to return. If your
-- results were truncated, you can make a subsequent pagination request
-- using the @Marker@ request parameter to retrieve more items.
lakrsIsTruncated :: Lens' ListAccessKeysResponse (Maybe Bool)
lakrsIsTruncated = lens _lakrsIsTruncated (\ s a -> s{_lakrsIsTruncated = a});

-- | FIXME: Undocumented member.
lakrsStatus :: Lens' ListAccessKeysResponse Int
lakrsStatus = lens _lakrsStatus (\ s a -> s{_lakrsStatus = a});

-- | A list of access key metadata.
lakrsAccessKeyMetadata :: Lens' ListAccessKeysResponse [AccessKeyMetadata]
lakrsAccessKeyMetadata = lens _lakrsAccessKeyMetadata (\ s a -> s{_lakrsAccessKeyMetadata = a});
