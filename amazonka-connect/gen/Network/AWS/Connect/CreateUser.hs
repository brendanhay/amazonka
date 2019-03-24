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
-- Module      : Network.AWS.Connect.CreateUser
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new user account in your Amazon Connect instance.
--
--
module Network.AWS.Connect.CreateUser
    (
    -- * Creating a Request
      createUser
    , CreateUser
    -- * Request Lenses
    , cuDirectoryUserId
    , cuIdentityInfo
    , cuPassword
    , cuHierarchyGroupId
    , cuUsername
    , cuPhoneConfig
    , cuSecurityProfileIds
    , cuRoutingProfileId
    , cuInstanceId

    -- * Destructuring the Response
    , createUserResponse
    , CreateUserResponse
    -- * Response Lenses
    , cursUserId
    , cursUserARN
    , cursResponseStatus
    ) where

import Network.AWS.Connect.Types
import Network.AWS.Connect.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createUser' smart constructor.
data CreateUser = CreateUser'
  { _cuDirectoryUserId    :: !(Maybe Text)
  , _cuIdentityInfo       :: !(Maybe UserIdentityInfo)
  , _cuPassword           :: !(Maybe Text)
  , _cuHierarchyGroupId   :: !(Maybe Text)
  , _cuUsername           :: !Text
  , _cuPhoneConfig        :: !UserPhoneConfig
  , _cuSecurityProfileIds :: !(List1 Text)
  , _cuRoutingProfileId   :: !Text
  , _cuInstanceId         :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateUser' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cuDirectoryUserId' - The unique identifier for the user account in the directory service directory used for identity management. If Amazon Connect is unable to access the existing directory, you can use the @DirectoryUserId@ to authenticate users. If you include the parameter, it is assumed that Amazon Connect cannot access the directory. If the parameter is not included, the @UserIdentityInfo@ is used to authenticate users from your existing directory. This parameter is required if you are using an existing directory for identity management in Amazon Connect when Amazon Connect cannot access your directory to authenticate users. If you are using SAML for identity management and include this parameter, an @InvalidRequestException@ is returned.
--
-- * 'cuIdentityInfo' - Information about the user, including email address, first name, and last name.
--
-- * 'cuPassword' - The password for the user account to create. This is required if you are using Amazon Connect for identity management. If you are using SAML for identity management and include this parameter, an @InvalidRequestException@ is returned.
--
-- * 'cuHierarchyGroupId' - The unique identifier for the hierarchy group to assign to the user created.
--
-- * 'cuUsername' - The user name in Amazon Connect for the account to create. If you are using SAML for identity management in your Amazon Connect, the value for @Username@ can include up to 64 characters from [a-zA-Z0-9_-.\@]+.
--
-- * 'cuPhoneConfig' - Specifies the phone settings for the user, including @AfterContactWorkTimeLimit@ , @AutoAccept@ , @DeskPhoneNumber@ , and @PhoneType@ .
--
-- * 'cuSecurityProfileIds' - The unique identifier of the security profile to assign to the user created.
--
-- * 'cuRoutingProfileId' - The unique identifier for the routing profile to assign to the user created.
--
-- * 'cuInstanceId' - The identifier for your Amazon Connect instance. To find the ID of your instance, open the AWS console and select Amazon Connect. Select the alias of the instance in the Instance alias column. The instance ID is displayed in the Overview section of your instance settings. For example, the instance ID is the set of characters at the end of the instance ARN, after instance/, such as 10a4c4eb-f57e-4d4c-b602-bf39176ced07.
createUser
    :: Text -- ^ 'cuUsername'
    -> UserPhoneConfig -- ^ 'cuPhoneConfig'
    -> NonEmpty Text -- ^ 'cuSecurityProfileIds'
    -> Text -- ^ 'cuRoutingProfileId'
    -> Text -- ^ 'cuInstanceId'
    -> CreateUser
createUser pUsername_ pPhoneConfig_ pSecurityProfileIds_ pRoutingProfileId_ pInstanceId_ =
  CreateUser'
    { _cuDirectoryUserId = Nothing
    , _cuIdentityInfo = Nothing
    , _cuPassword = Nothing
    , _cuHierarchyGroupId = Nothing
    , _cuUsername = pUsername_
    , _cuPhoneConfig = pPhoneConfig_
    , _cuSecurityProfileIds = _List1 # pSecurityProfileIds_
    , _cuRoutingProfileId = pRoutingProfileId_
    , _cuInstanceId = pInstanceId_
    }


-- | The unique identifier for the user account in the directory service directory used for identity management. If Amazon Connect is unable to access the existing directory, you can use the @DirectoryUserId@ to authenticate users. If you include the parameter, it is assumed that Amazon Connect cannot access the directory. If the parameter is not included, the @UserIdentityInfo@ is used to authenticate users from your existing directory. This parameter is required if you are using an existing directory for identity management in Amazon Connect when Amazon Connect cannot access your directory to authenticate users. If you are using SAML for identity management and include this parameter, an @InvalidRequestException@ is returned.
cuDirectoryUserId :: Lens' CreateUser (Maybe Text)
cuDirectoryUserId = lens _cuDirectoryUserId (\ s a -> s{_cuDirectoryUserId = a})

-- | Information about the user, including email address, first name, and last name.
cuIdentityInfo :: Lens' CreateUser (Maybe UserIdentityInfo)
cuIdentityInfo = lens _cuIdentityInfo (\ s a -> s{_cuIdentityInfo = a})

-- | The password for the user account to create. This is required if you are using Amazon Connect for identity management. If you are using SAML for identity management and include this parameter, an @InvalidRequestException@ is returned.
cuPassword :: Lens' CreateUser (Maybe Text)
cuPassword = lens _cuPassword (\ s a -> s{_cuPassword = a})

-- | The unique identifier for the hierarchy group to assign to the user created.
cuHierarchyGroupId :: Lens' CreateUser (Maybe Text)
cuHierarchyGroupId = lens _cuHierarchyGroupId (\ s a -> s{_cuHierarchyGroupId = a})

-- | The user name in Amazon Connect for the account to create. If you are using SAML for identity management in your Amazon Connect, the value for @Username@ can include up to 64 characters from [a-zA-Z0-9_-.\@]+.
cuUsername :: Lens' CreateUser Text
cuUsername = lens _cuUsername (\ s a -> s{_cuUsername = a})

-- | Specifies the phone settings for the user, including @AfterContactWorkTimeLimit@ , @AutoAccept@ , @DeskPhoneNumber@ , and @PhoneType@ .
cuPhoneConfig :: Lens' CreateUser UserPhoneConfig
cuPhoneConfig = lens _cuPhoneConfig (\ s a -> s{_cuPhoneConfig = a})

-- | The unique identifier of the security profile to assign to the user created.
cuSecurityProfileIds :: Lens' CreateUser (NonEmpty Text)
cuSecurityProfileIds = lens _cuSecurityProfileIds (\ s a -> s{_cuSecurityProfileIds = a}) . _List1

-- | The unique identifier for the routing profile to assign to the user created.
cuRoutingProfileId :: Lens' CreateUser Text
cuRoutingProfileId = lens _cuRoutingProfileId (\ s a -> s{_cuRoutingProfileId = a})

-- | The identifier for your Amazon Connect instance. To find the ID of your instance, open the AWS console and select Amazon Connect. Select the alias of the instance in the Instance alias column. The instance ID is displayed in the Overview section of your instance settings. For example, the instance ID is the set of characters at the end of the instance ARN, after instance/, such as 10a4c4eb-f57e-4d4c-b602-bf39176ced07.
cuInstanceId :: Lens' CreateUser Text
cuInstanceId = lens _cuInstanceId (\ s a -> s{_cuInstanceId = a})

instance AWSRequest CreateUser where
        type Rs CreateUser = CreateUserResponse
        request = putJSON connect
        response
          = receiveJSON
              (\ s h x ->
                 CreateUserResponse' <$>
                   (x .?> "UserId") <*> (x .?> "UserArn") <*>
                     (pure (fromEnum s)))

instance Hashable CreateUser where

instance NFData CreateUser where

instance ToHeaders CreateUser where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateUser where
        toJSON CreateUser'{..}
          = object
              (catMaybes
                 [("DirectoryUserId" .=) <$> _cuDirectoryUserId,
                  ("IdentityInfo" .=) <$> _cuIdentityInfo,
                  ("Password" .=) <$> _cuPassword,
                  ("HierarchyGroupId" .=) <$> _cuHierarchyGroupId,
                  Just ("Username" .= _cuUsername),
                  Just ("PhoneConfig" .= _cuPhoneConfig),
                  Just ("SecurityProfileIds" .= _cuSecurityProfileIds),
                  Just ("RoutingProfileId" .= _cuRoutingProfileId)])

instance ToPath CreateUser where
        toPath CreateUser'{..}
          = mconcat ["/users/", toBS _cuInstanceId]

instance ToQuery CreateUser where
        toQuery = const mempty

-- | /See:/ 'createUserResponse' smart constructor.
data CreateUserResponse = CreateUserResponse'
  { _cursUserId         :: !(Maybe Text)
  , _cursUserARN        :: !(Maybe Text)
  , _cursResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateUserResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cursUserId' - The unique identifier for the user account in Amazon Connect
--
-- * 'cursUserARN' - The Amazon Resource Name (ARN) of the user account created.
--
-- * 'cursResponseStatus' - -- | The response status code.
createUserResponse
    :: Int -- ^ 'cursResponseStatus'
    -> CreateUserResponse
createUserResponse pResponseStatus_ =
  CreateUserResponse'
    { _cursUserId = Nothing
    , _cursUserARN = Nothing
    , _cursResponseStatus = pResponseStatus_
    }


-- | The unique identifier for the user account in Amazon Connect
cursUserId :: Lens' CreateUserResponse (Maybe Text)
cursUserId = lens _cursUserId (\ s a -> s{_cursUserId = a})

-- | The Amazon Resource Name (ARN) of the user account created.
cursUserARN :: Lens' CreateUserResponse (Maybe Text)
cursUserARN = lens _cursUserARN (\ s a -> s{_cursUserARN = a})

-- | -- | The response status code.
cursResponseStatus :: Lens' CreateUserResponse Int
cursResponseStatus = lens _cursResponseStatus (\ s a -> s{_cursResponseStatus = a})

instance NFData CreateUserResponse where
