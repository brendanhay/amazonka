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
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the access key IDs associated with the
-- specified user. If there are none, the action returns an empty list.
--
-- Although each user is limited to a small number of keys, you can still
-- paginate the results using the 'MaxItems' and 'Marker' parameters.
--
-- If the 'UserName' field is not specified, the UserName is determined
-- implicitly based on the AWS access key ID used to sign the request.
-- Because this action works for access keys under the AWS account, you can
-- use this action to manage root credentials even if the AWS account has
-- no associated users.
--
-- To ensure the security of your AWS account, the secret access key is
-- accessible only during key and user creation.
--
-- /See:/ <http://docs.aws.amazon.com/IAM/latest/APIReference/API_ListAccessKeys.html AWS API Reference> for ListAccessKeys.
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

import           Network.AWS.IAM.Types
import           Network.AWS.IAM.Types.Product
import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'listAccessKeys' smart constructor.
data ListAccessKeys = ListAccessKeys'
    { _lakUserName :: !(Maybe Text)
    , _lakMarker   :: !(Maybe Text)
    , _lakMaxItems :: !(Maybe Nat)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListAccessKeys' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lakUserName'
--
-- * 'lakMarker'
--
-- * 'lakMaxItems'
listAccessKeys
    :: ListAccessKeys
listAccessKeys =
    ListAccessKeys'
    { _lakUserName = Nothing
    , _lakMarker = Nothing
    , _lakMaxItems = Nothing
    }

-- | The name of the user.
lakUserName :: Lens' ListAccessKeys (Maybe Text)
lakUserName = lens _lakUserName (\ s a -> s{_lakUserName = a});

-- | Use this parameter only when paginating results and only after you have
-- received a response where the results are truncated. Set it to the value
-- of the 'Marker' element in the response you just received.
lakMarker :: Lens' ListAccessKeys (Maybe Text)
lakMarker = lens _lakMarker (\ s a -> s{_lakMarker = a});

-- | Use this only when paginating results to indicate the maximum number of
-- items you want in the response. If there are additional items beyond the
-- maximum you specify, the 'IsTruncated' response element is 'true'.
--
-- This parameter is optional. If you do not include it, it defaults to
-- 100.
lakMaxItems :: Lens' ListAccessKeys (Maybe Natural)
lakMaxItems = lens _lakMaxItems (\ s a -> s{_lakMaxItems = a}) . mapping _Nat;

instance AWSPager ListAccessKeys where
        page rq rs
          | stop (rs ^. lakrsMarker) = Nothing
          | stop (rs ^. lakrsAccessKeyMetadata) = Nothing
          | otherwise =
            Just $ rq & lakMarker .~ rs ^. lakrsMarker

instance AWSRequest ListAccessKeys where
        type Rs ListAccessKeys = ListAccessKeysResponse
        request = postQuery iAM
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
               "UserName" =: _lakUserName, "Marker" =: _lakMarker,
               "MaxItems" =: _lakMaxItems]

-- | Contains the response to a successful ListAccessKeys request.
--
-- /See:/ 'listAccessKeysResponse' smart constructor.
data ListAccessKeysResponse = ListAccessKeysResponse'
    { _lakrsMarker            :: !(Maybe Text)
    , _lakrsIsTruncated       :: !(Maybe Bool)
    , _lakrsResponseStatus    :: !Int
    , _lakrsAccessKeyMetadata :: ![AccessKeyMetadata]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListAccessKeysResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lakrsMarker'
--
-- * 'lakrsIsTruncated'
--
-- * 'lakrsResponseStatus'
--
-- * 'lakrsAccessKeyMetadata'
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

-- | When 'IsTruncated' is 'true', this element is present and contains the
-- value to use for the 'Marker' parameter in a subsequent pagination
-- request.
lakrsMarker :: Lens' ListAccessKeysResponse (Maybe Text)
lakrsMarker = lens _lakrsMarker (\ s a -> s{_lakrsMarker = a});

-- | A flag that indicates whether there are more items to return. If your
-- results were truncated, you can make a subsequent pagination request
-- using the 'Marker' request parameter to retrieve more items.
lakrsIsTruncated :: Lens' ListAccessKeysResponse (Maybe Bool)
lakrsIsTruncated = lens _lakrsIsTruncated (\ s a -> s{_lakrsIsTruncated = a});

-- | The response status code.
lakrsResponseStatus :: Lens' ListAccessKeysResponse Int
lakrsResponseStatus = lens _lakrsResponseStatus (\ s a -> s{_lakrsResponseStatus = a});

-- | A list of access key metadata.
lakrsAccessKeyMetadata :: Lens' ListAccessKeysResponse [AccessKeyMetadata]
lakrsAccessKeyMetadata = lens _lakrsAccessKeyMetadata (\ s a -> s{_lakrsAccessKeyMetadata = a}) . _Coerce;
