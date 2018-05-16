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
-- Module      : Network.AWS.CodeStar.CreateUserProfile
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a profile for a user that includes user preferences, such as the display name and email address assocciated with the user, in AWS CodeStar. The user profile is not project-specific. Information in the user profile is displayed wherever the user's information appears to other users in AWS CodeStar.
--
--
module Network.AWS.CodeStar.CreateUserProfile
    (
    -- * Creating a Request
      createUserProfile
    , CreateUserProfile
    -- * Request Lenses
    , cupSshPublicKey
    , cupUserARN
    , cupDisplayName
    , cupEmailAddress

    -- * Destructuring the Response
    , createUserProfileResponse
    , CreateUserProfileResponse
    -- * Response Lenses
    , cuprsLastModifiedTimestamp
    , cuprsSshPublicKey
    , cuprsEmailAddress
    , cuprsDisplayName
    , cuprsCreatedTimestamp
    , cuprsResponseStatus
    , cuprsUserARN
    ) where

import Network.AWS.CodeStar.Types
import Network.AWS.CodeStar.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createUserProfile' smart constructor.
data CreateUserProfile = CreateUserProfile'
  { _cupSshPublicKey :: !(Maybe Text)
  , _cupUserARN      :: !Text
  , _cupDisplayName  :: !Text
  , _cupEmailAddress :: !(Sensitive Text)
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateUserProfile' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cupSshPublicKey' - The SSH public key associated with the user in AWS CodeStar. If a project owner allows the user remote access to project resources, this public key will be used along with the user's private key for SSH access.
--
-- * 'cupUserARN' - The Amazon Resource Name (ARN) of the user in IAM.
--
-- * 'cupDisplayName' - The name that will be displayed as the friendly name for the user in AWS CodeStar.
--
-- * 'cupEmailAddress' - The email address that will be displayed as part of the user's profile in AWS CodeStar.
createUserProfile
    :: Text -- ^ 'cupUserARN'
    -> Text -- ^ 'cupDisplayName'
    -> Text -- ^ 'cupEmailAddress'
    -> CreateUserProfile
createUserProfile pUserARN_ pDisplayName_ pEmailAddress_ =
  CreateUserProfile'
    { _cupSshPublicKey = Nothing
    , _cupUserARN = pUserARN_
    , _cupDisplayName = pDisplayName_
    , _cupEmailAddress = _Sensitive # pEmailAddress_
    }


-- | The SSH public key associated with the user in AWS CodeStar. If a project owner allows the user remote access to project resources, this public key will be used along with the user's private key for SSH access.
cupSshPublicKey :: Lens' CreateUserProfile (Maybe Text)
cupSshPublicKey = lens _cupSshPublicKey (\ s a -> s{_cupSshPublicKey = a})

-- | The Amazon Resource Name (ARN) of the user in IAM.
cupUserARN :: Lens' CreateUserProfile Text
cupUserARN = lens _cupUserARN (\ s a -> s{_cupUserARN = a})

-- | The name that will be displayed as the friendly name for the user in AWS CodeStar.
cupDisplayName :: Lens' CreateUserProfile Text
cupDisplayName = lens _cupDisplayName (\ s a -> s{_cupDisplayName = a})

-- | The email address that will be displayed as part of the user's profile in AWS CodeStar.
cupEmailAddress :: Lens' CreateUserProfile Text
cupEmailAddress = lens _cupEmailAddress (\ s a -> s{_cupEmailAddress = a}) . _Sensitive

instance AWSRequest CreateUserProfile where
        type Rs CreateUserProfile = CreateUserProfileResponse
        request = postJSON codeStar
        response
          = receiveJSON
              (\ s h x ->
                 CreateUserProfileResponse' <$>
                   (x .?> "lastModifiedTimestamp") <*>
                     (x .?> "sshPublicKey")
                     <*> (x .?> "emailAddress")
                     <*> (x .?> "displayName")
                     <*> (x .?> "createdTimestamp")
                     <*> (pure (fromEnum s))
                     <*> (x .:> "userArn"))

instance Hashable CreateUserProfile where

instance NFData CreateUserProfile where

instance ToHeaders CreateUserProfile where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CodeStar_20170419.CreateUserProfile" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateUserProfile where
        toJSON CreateUserProfile'{..}
          = object
              (catMaybes
                 [("sshPublicKey" .=) <$> _cupSshPublicKey,
                  Just ("userArn" .= _cupUserARN),
                  Just ("displayName" .= _cupDisplayName),
                  Just ("emailAddress" .= _cupEmailAddress)])

instance ToPath CreateUserProfile where
        toPath = const "/"

instance ToQuery CreateUserProfile where
        toQuery = const mempty

-- | /See:/ 'createUserProfileResponse' smart constructor.
data CreateUserProfileResponse = CreateUserProfileResponse'
  { _cuprsLastModifiedTimestamp :: !(Maybe POSIX)
  , _cuprsSshPublicKey          :: !(Maybe Text)
  , _cuprsEmailAddress          :: !(Maybe (Sensitive Text))
  , _cuprsDisplayName           :: !(Maybe Text)
  , _cuprsCreatedTimestamp      :: !(Maybe POSIX)
  , _cuprsResponseStatus        :: !Int
  , _cuprsUserARN               :: !Text
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateUserProfileResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cuprsLastModifiedTimestamp' - The date the user profile was last modified, in timestamp format.
--
-- * 'cuprsSshPublicKey' - The SSH public key associated with the user in AWS CodeStar. This is the public portion of the public/private keypair the user can use to access project resources if a project owner allows the user remote access to those resources.
--
-- * 'cuprsEmailAddress' - The email address that is displayed as part of the user's profile in AWS CodeStar.
--
-- * 'cuprsDisplayName' - The name that is displayed as the friendly name for the user in AWS CodeStar.
--
-- * 'cuprsCreatedTimestamp' - The date the user profile was created, in timestamp format.
--
-- * 'cuprsResponseStatus' - -- | The response status code.
--
-- * 'cuprsUserARN' - The Amazon Resource Name (ARN) of the user in IAM.
createUserProfileResponse
    :: Int -- ^ 'cuprsResponseStatus'
    -> Text -- ^ 'cuprsUserARN'
    -> CreateUserProfileResponse
createUserProfileResponse pResponseStatus_ pUserARN_ =
  CreateUserProfileResponse'
    { _cuprsLastModifiedTimestamp = Nothing
    , _cuprsSshPublicKey = Nothing
    , _cuprsEmailAddress = Nothing
    , _cuprsDisplayName = Nothing
    , _cuprsCreatedTimestamp = Nothing
    , _cuprsResponseStatus = pResponseStatus_
    , _cuprsUserARN = pUserARN_
    }


-- | The date the user profile was last modified, in timestamp format.
cuprsLastModifiedTimestamp :: Lens' CreateUserProfileResponse (Maybe UTCTime)
cuprsLastModifiedTimestamp = lens _cuprsLastModifiedTimestamp (\ s a -> s{_cuprsLastModifiedTimestamp = a}) . mapping _Time

-- | The SSH public key associated with the user in AWS CodeStar. This is the public portion of the public/private keypair the user can use to access project resources if a project owner allows the user remote access to those resources.
cuprsSshPublicKey :: Lens' CreateUserProfileResponse (Maybe Text)
cuprsSshPublicKey = lens _cuprsSshPublicKey (\ s a -> s{_cuprsSshPublicKey = a})

-- | The email address that is displayed as part of the user's profile in AWS CodeStar.
cuprsEmailAddress :: Lens' CreateUserProfileResponse (Maybe Text)
cuprsEmailAddress = lens _cuprsEmailAddress (\ s a -> s{_cuprsEmailAddress = a}) . mapping _Sensitive

-- | The name that is displayed as the friendly name for the user in AWS CodeStar.
cuprsDisplayName :: Lens' CreateUserProfileResponse (Maybe Text)
cuprsDisplayName = lens _cuprsDisplayName (\ s a -> s{_cuprsDisplayName = a})

-- | The date the user profile was created, in timestamp format.
cuprsCreatedTimestamp :: Lens' CreateUserProfileResponse (Maybe UTCTime)
cuprsCreatedTimestamp = lens _cuprsCreatedTimestamp (\ s a -> s{_cuprsCreatedTimestamp = a}) . mapping _Time

-- | -- | The response status code.
cuprsResponseStatus :: Lens' CreateUserProfileResponse Int
cuprsResponseStatus = lens _cuprsResponseStatus (\ s a -> s{_cuprsResponseStatus = a})

-- | The Amazon Resource Name (ARN) of the user in IAM.
cuprsUserARN :: Lens' CreateUserProfileResponse Text
cuprsUserARN = lens _cuprsUserARN (\ s a -> s{_cuprsUserARN = a})

instance NFData CreateUserProfileResponse where
