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
-- Module      : Network.AWS.CodeStar.UpdateUserProfile
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a user's profile in AWS CodeStar. The user profile is not project-specific. Information in the user profile is displayed wherever the user's information appears to other users in AWS CodeStar.
--
--
module Network.AWS.CodeStar.UpdateUserProfile
    (
    -- * Creating a Request
      updateUserProfile
    , UpdateUserProfile
    -- * Request Lenses
    , uupSshPublicKey
    , uupEmailAddress
    , uupDisplayName
    , uupUserARN

    -- * Destructuring the Response
    , updateUserProfileResponse
    , UpdateUserProfileResponse
    -- * Response Lenses
    , uuprsLastModifiedTimestamp
    , uuprsSshPublicKey
    , uuprsEmailAddress
    , uuprsDisplayName
    , uuprsCreatedTimestamp
    , uuprsResponseStatus
    , uuprsUserARN
    ) where

import Network.AWS.CodeStar.Types
import Network.AWS.CodeStar.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateUserProfile' smart constructor.
data UpdateUserProfile = UpdateUserProfile'
  { _uupSshPublicKey :: !(Maybe Text)
  , _uupEmailAddress :: !(Maybe (Sensitive Text))
  , _uupDisplayName  :: !(Maybe Text)
  , _uupUserARN      :: !Text
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateUserProfile' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uupSshPublicKey' - The SSH public key associated with the user in AWS CodeStar. If a project owner allows the user remote access to project resources, this public key will be used along with the user's private key for SSH access.
--
-- * 'uupEmailAddress' - The email address that is displayed as part of the user's profile in AWS CodeStar.
--
-- * 'uupDisplayName' - The name that is displayed as the friendly name for the user in AWS CodeStar.
--
-- * 'uupUserARN' - The name that will be displayed as the friendly name for the user in AWS CodeStar.
updateUserProfile
    :: Text -- ^ 'uupUserARN'
    -> UpdateUserProfile
updateUserProfile pUserARN_ =
  UpdateUserProfile'
    { _uupSshPublicKey = Nothing
    , _uupEmailAddress = Nothing
    , _uupDisplayName = Nothing
    , _uupUserARN = pUserARN_
    }


-- | The SSH public key associated with the user in AWS CodeStar. If a project owner allows the user remote access to project resources, this public key will be used along with the user's private key for SSH access.
uupSshPublicKey :: Lens' UpdateUserProfile (Maybe Text)
uupSshPublicKey = lens _uupSshPublicKey (\ s a -> s{_uupSshPublicKey = a})

-- | The email address that is displayed as part of the user's profile in AWS CodeStar.
uupEmailAddress :: Lens' UpdateUserProfile (Maybe Text)
uupEmailAddress = lens _uupEmailAddress (\ s a -> s{_uupEmailAddress = a}) . mapping _Sensitive

-- | The name that is displayed as the friendly name for the user in AWS CodeStar.
uupDisplayName :: Lens' UpdateUserProfile (Maybe Text)
uupDisplayName = lens _uupDisplayName (\ s a -> s{_uupDisplayName = a})

-- | The name that will be displayed as the friendly name for the user in AWS CodeStar.
uupUserARN :: Lens' UpdateUserProfile Text
uupUserARN = lens _uupUserARN (\ s a -> s{_uupUserARN = a})

instance AWSRequest UpdateUserProfile where
        type Rs UpdateUserProfile = UpdateUserProfileResponse
        request = postJSON codeStar
        response
          = receiveJSON
              (\ s h x ->
                 UpdateUserProfileResponse' <$>
                   (x .?> "lastModifiedTimestamp") <*>
                     (x .?> "sshPublicKey")
                     <*> (x .?> "emailAddress")
                     <*> (x .?> "displayName")
                     <*> (x .?> "createdTimestamp")
                     <*> (pure (fromEnum s))
                     <*> (x .:> "userArn"))

instance Hashable UpdateUserProfile where

instance NFData UpdateUserProfile where

instance ToHeaders UpdateUserProfile where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CodeStar_20170419.UpdateUserProfile" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateUserProfile where
        toJSON UpdateUserProfile'{..}
          = object
              (catMaybes
                 [("sshPublicKey" .=) <$> _uupSshPublicKey,
                  ("emailAddress" .=) <$> _uupEmailAddress,
                  ("displayName" .=) <$> _uupDisplayName,
                  Just ("userArn" .= _uupUserARN)])

instance ToPath UpdateUserProfile where
        toPath = const "/"

instance ToQuery UpdateUserProfile where
        toQuery = const mempty

-- | /See:/ 'updateUserProfileResponse' smart constructor.
data UpdateUserProfileResponse = UpdateUserProfileResponse'
  { _uuprsLastModifiedTimestamp :: !(Maybe POSIX)
  , _uuprsSshPublicKey          :: !(Maybe Text)
  , _uuprsEmailAddress          :: !(Maybe (Sensitive Text))
  , _uuprsDisplayName           :: !(Maybe Text)
  , _uuprsCreatedTimestamp      :: !(Maybe POSIX)
  , _uuprsResponseStatus        :: !Int
  , _uuprsUserARN               :: !Text
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateUserProfileResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uuprsLastModifiedTimestamp' - The date the user profile was last modified, in timestamp format.
--
-- * 'uuprsSshPublicKey' - The SSH public key associated with the user in AWS CodeStar. This is the public portion of the public/private keypair the user can use to access project resources if a project owner allows the user remote access to those resources.
--
-- * 'uuprsEmailAddress' - The email address that is displayed as part of the user's profile in AWS CodeStar.
--
-- * 'uuprsDisplayName' - The name that is displayed as the friendly name for the user in AWS CodeStar.
--
-- * 'uuprsCreatedTimestamp' - The date the user profile was created, in timestamp format.
--
-- * 'uuprsResponseStatus' - -- | The response status code.
--
-- * 'uuprsUserARN' - The Amazon Resource Name (ARN) of the user in IAM.
updateUserProfileResponse
    :: Int -- ^ 'uuprsResponseStatus'
    -> Text -- ^ 'uuprsUserARN'
    -> UpdateUserProfileResponse
updateUserProfileResponse pResponseStatus_ pUserARN_ =
  UpdateUserProfileResponse'
    { _uuprsLastModifiedTimestamp = Nothing
    , _uuprsSshPublicKey = Nothing
    , _uuprsEmailAddress = Nothing
    , _uuprsDisplayName = Nothing
    , _uuprsCreatedTimestamp = Nothing
    , _uuprsResponseStatus = pResponseStatus_
    , _uuprsUserARN = pUserARN_
    }


-- | The date the user profile was last modified, in timestamp format.
uuprsLastModifiedTimestamp :: Lens' UpdateUserProfileResponse (Maybe UTCTime)
uuprsLastModifiedTimestamp = lens _uuprsLastModifiedTimestamp (\ s a -> s{_uuprsLastModifiedTimestamp = a}) . mapping _Time

-- | The SSH public key associated with the user in AWS CodeStar. This is the public portion of the public/private keypair the user can use to access project resources if a project owner allows the user remote access to those resources.
uuprsSshPublicKey :: Lens' UpdateUserProfileResponse (Maybe Text)
uuprsSshPublicKey = lens _uuprsSshPublicKey (\ s a -> s{_uuprsSshPublicKey = a})

-- | The email address that is displayed as part of the user's profile in AWS CodeStar.
uuprsEmailAddress :: Lens' UpdateUserProfileResponse (Maybe Text)
uuprsEmailAddress = lens _uuprsEmailAddress (\ s a -> s{_uuprsEmailAddress = a}) . mapping _Sensitive

-- | The name that is displayed as the friendly name for the user in AWS CodeStar.
uuprsDisplayName :: Lens' UpdateUserProfileResponse (Maybe Text)
uuprsDisplayName = lens _uuprsDisplayName (\ s a -> s{_uuprsDisplayName = a})

-- | The date the user profile was created, in timestamp format.
uuprsCreatedTimestamp :: Lens' UpdateUserProfileResponse (Maybe UTCTime)
uuprsCreatedTimestamp = lens _uuprsCreatedTimestamp (\ s a -> s{_uuprsCreatedTimestamp = a}) . mapping _Time

-- | -- | The response status code.
uuprsResponseStatus :: Lens' UpdateUserProfileResponse Int
uuprsResponseStatus = lens _uuprsResponseStatus (\ s a -> s{_uuprsResponseStatus = a})

-- | The Amazon Resource Name (ARN) of the user in IAM.
uuprsUserARN :: Lens' UpdateUserProfileResponse Text
uuprsUserARN = lens _uuprsUserARN (\ s a -> s{_uuprsUserARN = a})

instance NFData UpdateUserProfileResponse where
