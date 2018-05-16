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
-- Module      : Network.AWS.IAM.ListSSHPublicKeys
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the SSH public keys associated with the specified IAM user. If there are none, the operation returns an empty list.
--
--
-- The SSH public keys returned by this operation are used only for authenticating the IAM user to an AWS CodeCommit repository. For more information about using SSH keys to authenticate to an AWS CodeCommit repository, see <http://docs.aws.amazon.com/codecommit/latest/userguide/setting-up-credentials-ssh.html Set up AWS CodeCommit for SSH Connections> in the /AWS CodeCommit User Guide/ .
--
-- Although each user is limited to a small number of keys, you can still paginate the results using the @MaxItems@ and @Marker@ parameters.
--
--
-- This operation returns paginated results.
module Network.AWS.IAM.ListSSHPublicKeys
    (
    -- * Creating a Request
      listSSHPublicKeys
    , ListSSHPublicKeys
    -- * Request Lenses
    , lspkUserName
    , lspkMarker
    , lspkMaxItems

    -- * Destructuring the Response
    , listSSHPublicKeysResponse
    , ListSSHPublicKeysResponse
    -- * Response Lenses
    , lspkrsSSHPublicKeys
    , lspkrsMarker
    , lspkrsIsTruncated
    , lspkrsResponseStatus
    ) where

import Network.AWS.IAM.Types
import Network.AWS.IAM.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listSSHPublicKeys' smart constructor.
data ListSSHPublicKeys = ListSSHPublicKeys'
  { _lspkUserName :: !(Maybe Text)
  , _lspkMarker   :: !(Maybe Text)
  , _lspkMaxItems :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListSSHPublicKeys' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lspkUserName' - The name of the IAM user to list SSH public keys for. If none is specified, the @UserName@ field is determined implicitly based on the AWS access key used to sign the request. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- * 'lspkMarker' - Use this parameter only when paginating results and only after you receive a response indicating that the results are truncated. Set it to the value of the @Marker@ element in the response that you received to indicate where the next call should start.
--
-- * 'lspkMaxItems' - (Optional) Use this only when paginating results to indicate the maximum number of items you want in the response. If additional items exist beyond the maximum you specify, the @IsTruncated@ response element is @true@ . If you do not include this parameter, it defaults to 100. Note that IAM might return fewer results, even when there are more results available. In that case, the @IsTruncated@ response element returns @true@ and @Marker@ contains a value to include in the subsequent call that tells the service where to continue from.
listSSHPublicKeys
    :: ListSSHPublicKeys
listSSHPublicKeys =
  ListSSHPublicKeys'
    {_lspkUserName = Nothing, _lspkMarker = Nothing, _lspkMaxItems = Nothing}


-- | The name of the IAM user to list SSH public keys for. If none is specified, the @UserName@ field is determined implicitly based on the AWS access key used to sign the request. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
lspkUserName :: Lens' ListSSHPublicKeys (Maybe Text)
lspkUserName = lens _lspkUserName (\ s a -> s{_lspkUserName = a})

-- | Use this parameter only when paginating results and only after you receive a response indicating that the results are truncated. Set it to the value of the @Marker@ element in the response that you received to indicate where the next call should start.
lspkMarker :: Lens' ListSSHPublicKeys (Maybe Text)
lspkMarker = lens _lspkMarker (\ s a -> s{_lspkMarker = a})

-- | (Optional) Use this only when paginating results to indicate the maximum number of items you want in the response. If additional items exist beyond the maximum you specify, the @IsTruncated@ response element is @true@ . If you do not include this parameter, it defaults to 100. Note that IAM might return fewer results, even when there are more results available. In that case, the @IsTruncated@ response element returns @true@ and @Marker@ contains a value to include in the subsequent call that tells the service where to continue from.
lspkMaxItems :: Lens' ListSSHPublicKeys (Maybe Natural)
lspkMaxItems = lens _lspkMaxItems (\ s a -> s{_lspkMaxItems = a}) . mapping _Nat

instance AWSPager ListSSHPublicKeys where
        page rq rs
          | stop (rs ^. lspkrsIsTruncated) = Nothing
          | isNothing (rs ^. lspkrsMarker) = Nothing
          | otherwise =
            Just $ rq & lspkMarker .~ rs ^. lspkrsMarker

instance AWSRequest ListSSHPublicKeys where
        type Rs ListSSHPublicKeys = ListSSHPublicKeysResponse
        request = postQuery iam
        response
          = receiveXMLWrapper "ListSSHPublicKeysResult"
              (\ s h x ->
                 ListSSHPublicKeysResponse' <$>
                   (x .@? "SSHPublicKeys" .!@ mempty >>=
                      may (parseXMLList "member"))
                     <*> (x .@? "Marker")
                     <*> (x .@? "IsTruncated")
                     <*> (pure (fromEnum s)))

instance Hashable ListSSHPublicKeys where

instance NFData ListSSHPublicKeys where

instance ToHeaders ListSSHPublicKeys where
        toHeaders = const mempty

instance ToPath ListSSHPublicKeys where
        toPath = const "/"

instance ToQuery ListSSHPublicKeys where
        toQuery ListSSHPublicKeys'{..}
          = mconcat
              ["Action" =: ("ListSSHPublicKeys" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "UserName" =: _lspkUserName, "Marker" =: _lspkMarker,
               "MaxItems" =: _lspkMaxItems]

-- | Contains the response to a successful 'ListSSHPublicKeys' request.
--
--
--
-- /See:/ 'listSSHPublicKeysResponse' smart constructor.
data ListSSHPublicKeysResponse = ListSSHPublicKeysResponse'
  { _lspkrsSSHPublicKeys  :: !(Maybe [SSHPublicKeyMetadata])
  , _lspkrsMarker         :: !(Maybe Text)
  , _lspkrsIsTruncated    :: !(Maybe Bool)
  , _lspkrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListSSHPublicKeysResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lspkrsSSHPublicKeys' - A list of the SSH public keys assigned to IAM user.
--
-- * 'lspkrsMarker' - When @IsTruncated@ is @true@ , this element is present and contains the value to use for the @Marker@ parameter in a subsequent pagination request.
--
-- * 'lspkrsIsTruncated' - A flag that indicates whether there are more items to return. If your results were truncated, you can make a subsequent pagination request using the @Marker@ request parameter to retrieve more items. Note that IAM might return fewer than the @MaxItems@ number of results even when there are more results available. We recommend that you check @IsTruncated@ after every call to ensure that you receive all of your results.
--
-- * 'lspkrsResponseStatus' - -- | The response status code.
listSSHPublicKeysResponse
    :: Int -- ^ 'lspkrsResponseStatus'
    -> ListSSHPublicKeysResponse
listSSHPublicKeysResponse pResponseStatus_ =
  ListSSHPublicKeysResponse'
    { _lspkrsSSHPublicKeys = Nothing
    , _lspkrsMarker = Nothing
    , _lspkrsIsTruncated = Nothing
    , _lspkrsResponseStatus = pResponseStatus_
    }


-- | A list of the SSH public keys assigned to IAM user.
lspkrsSSHPublicKeys :: Lens' ListSSHPublicKeysResponse [SSHPublicKeyMetadata]
lspkrsSSHPublicKeys = lens _lspkrsSSHPublicKeys (\ s a -> s{_lspkrsSSHPublicKeys = a}) . _Default . _Coerce

-- | When @IsTruncated@ is @true@ , this element is present and contains the value to use for the @Marker@ parameter in a subsequent pagination request.
lspkrsMarker :: Lens' ListSSHPublicKeysResponse (Maybe Text)
lspkrsMarker = lens _lspkrsMarker (\ s a -> s{_lspkrsMarker = a})

-- | A flag that indicates whether there are more items to return. If your results were truncated, you can make a subsequent pagination request using the @Marker@ request parameter to retrieve more items. Note that IAM might return fewer than the @MaxItems@ number of results even when there are more results available. We recommend that you check @IsTruncated@ after every call to ensure that you receive all of your results.
lspkrsIsTruncated :: Lens' ListSSHPublicKeysResponse (Maybe Bool)
lspkrsIsTruncated = lens _lspkrsIsTruncated (\ s a -> s{_lspkrsIsTruncated = a})

-- | -- | The response status code.
lspkrsResponseStatus :: Lens' ListSSHPublicKeysResponse Int
lspkrsResponseStatus = lens _lspkrsResponseStatus (\ s a -> s{_lspkrsResponseStatus = a})

instance NFData ListSSHPublicKeysResponse where
