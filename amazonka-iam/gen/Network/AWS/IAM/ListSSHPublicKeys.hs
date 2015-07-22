{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.ListSSHPublicKeys
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the SSH public keys associated with the
-- specified IAM user. If there are none, the action returns an empty list.
--
-- The SSH public keys returned by this action are used only for
-- authenticating the IAM user to an AWS CodeCommit repository. For more
-- information about using SSH keys to authenticate to an AWS CodeCommit
-- repository, see
-- <http://docs.aws.amazon.com/codecommit/latest/userguide/setting-up-credentials-ssh.html Set up AWS CodeCommit for SSH Connections>
-- in the /AWS CodeCommit User Guide/.
--
-- Although each user is limited to a small number of keys, you can still
-- paginate the results using the @MaxItems@ and @Marker@ parameters.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_ListSSHPublicKeys.html>
module Network.AWS.IAM.ListSSHPublicKeys
    (
    -- * Request
      ListSSHPublicKeys
    -- ** Request constructor
    , listSSHPublicKeys
    -- ** Request lenses
    , lspkrqUserName
    , lspkrqMaxItems
    , lspkrqMarker

    -- * Response
    , ListSSHPublicKeysResponse
    -- ** Response constructor
    , listSSHPublicKeysResponse
    -- ** Response lenses
    , lspkrsSSHPublicKeys
    , lspkrsMarker
    , lspkrsIsTruncated
    , lspkrsStatus
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'listSSHPublicKeys' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lspkrqUserName'
--
-- * 'lspkrqMaxItems'
--
-- * 'lspkrqMarker'
data ListSSHPublicKeys = ListSSHPublicKeys'
    { _lspkrqUserName :: !(Maybe Text)
    , _lspkrqMaxItems :: !(Maybe Nat)
    , _lspkrqMarker   :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListSSHPublicKeys' smart constructor.
listSSHPublicKeys :: ListSSHPublicKeys
listSSHPublicKeys =
    ListSSHPublicKeys'
    { _lspkrqUserName = Nothing
    , _lspkrqMaxItems = Nothing
    , _lspkrqMarker = Nothing
    }

-- | The name of the IAM user to list SSH public keys for. If none is
-- specified, the UserName field is determined implicitly based on the AWS
-- access key used to sign the request.
lspkrqUserName :: Lens' ListSSHPublicKeys (Maybe Text)
lspkrqUserName = lens _lspkrqUserName (\ s a -> s{_lspkrqUserName = a});

-- | Use this only when paginating results to indicate the maximum number of
-- items you want in the response. If there are additional items beyond the
-- maximum you specify, the @IsTruncated@ response element is @true@.
--
-- This parameter is optional. If you do not include it, it defaults to
-- 100.
lspkrqMaxItems :: Lens' ListSSHPublicKeys (Maybe Natural)
lspkrqMaxItems = lens _lspkrqMaxItems (\ s a -> s{_lspkrqMaxItems = a}) . mapping _Nat;

-- | Use this parameter only when paginating results and only after you have
-- received a response where the results are truncated. Set it to the value
-- of the @Marker@ element in the response you just received.
lspkrqMarker :: Lens' ListSSHPublicKeys (Maybe Text)
lspkrqMarker = lens _lspkrqMarker (\ s a -> s{_lspkrqMarker = a});

instance AWSRequest ListSSHPublicKeys where
        type Sv ListSSHPublicKeys = IAM
        type Rs ListSSHPublicKeys = ListSSHPublicKeysResponse
        request = post
        response
          = receiveXMLWrapper "ListSSHPublicKeysResult"
              (\ s h x ->
                 ListSSHPublicKeysResponse' <$>
                   (x .@? "SSHPublicKeys" .!@ mempty >>=
                      may (parseXMLList "member"))
                     <*> (x .@? "Marker")
                     <*> (x .@? "IsTruncated")
                     <*> (pure (fromEnum s)))

instance ToHeaders ListSSHPublicKeys where
        toHeaders = const mempty

instance ToPath ListSSHPublicKeys where
        toPath = const "/"

instance ToQuery ListSSHPublicKeys where
        toQuery ListSSHPublicKeys'{..}
          = mconcat
              ["Action" =: ("ListSSHPublicKeys" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "UserName" =: _lspkrqUserName,
               "MaxItems" =: _lspkrqMaxItems,
               "Marker" =: _lspkrqMarker]

-- | Contains the response to a successful ListSSHPublicKeys request.
--
-- /See:/ 'listSSHPublicKeysResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lspkrsSSHPublicKeys'
--
-- * 'lspkrsMarker'
--
-- * 'lspkrsIsTruncated'
--
-- * 'lspkrsStatus'
data ListSSHPublicKeysResponse = ListSSHPublicKeysResponse'
    { _lspkrsSSHPublicKeys :: !(Maybe [SSHPublicKeyMetadata])
    , _lspkrsMarker        :: !(Maybe Text)
    , _lspkrsIsTruncated   :: !(Maybe Bool)
    , _lspkrsStatus        :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListSSHPublicKeysResponse' smart constructor.
listSSHPublicKeysResponse :: Int -> ListSSHPublicKeysResponse
listSSHPublicKeysResponse pStatus =
    ListSSHPublicKeysResponse'
    { _lspkrsSSHPublicKeys = Nothing
    , _lspkrsMarker = Nothing
    , _lspkrsIsTruncated = Nothing
    , _lspkrsStatus = pStatus
    }

-- | A list of SSH public keys.
lspkrsSSHPublicKeys :: Lens' ListSSHPublicKeysResponse [SSHPublicKeyMetadata]
lspkrsSSHPublicKeys = lens _lspkrsSSHPublicKeys (\ s a -> s{_lspkrsSSHPublicKeys = a}) . _Default;

-- | When @IsTruncated@ is @true@, this element is present and contains the
-- value to use for the @Marker@ parameter in a subsequent pagination
-- request.
lspkrsMarker :: Lens' ListSSHPublicKeysResponse (Maybe Text)
lspkrsMarker = lens _lspkrsMarker (\ s a -> s{_lspkrsMarker = a});

-- | A flag that indicates whether there are more items to return. If your
-- results were truncated, you can make a subsequent pagination request
-- using the @Marker@ request parameter to retrieve more items.
lspkrsIsTruncated :: Lens' ListSSHPublicKeysResponse (Maybe Bool)
lspkrsIsTruncated = lens _lspkrsIsTruncated (\ s a -> s{_lspkrsIsTruncated = a});

-- | FIXME: Undocumented member.
lspkrsStatus :: Lens' ListSSHPublicKeysResponse Int
lspkrsStatus = lens _lspkrsStatus (\ s a -> s{_lspkrsStatus = a});
