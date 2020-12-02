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
-- Module      : Network.AWS.CognitoIdentityProvider.AdminCreateUser
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new user in the specified user pool.
--
--
-- If @MessageAction@ is not set, the default is to send a welcome message via email or phone (SMS).
--
-- Alternatively, you can call AdminCreateUser with “SUPPRESS” for the @MessageAction@ parameter, and Amazon Cognito will not send any email.
--
-- In either case, the user will be in the @FORCE_CHANGE_PASSWORD@ state until they sign in and change their password.
--
-- AdminCreateUser requires developer credentials.
--
module Network.AWS.CognitoIdentityProvider.AdminCreateUser
    (
    -- * Creating a Request
      adminCreateUser
    , AdminCreateUser
    -- * Request Lenses
    , acuTemporaryPassword
    , acuForceAliasCreation
    , acuDesiredDeliveryMediums
    , acuMessageAction
    , acuUserAttributes
    , acuValidationData
    , acuUserPoolId
    , acuUsername

    -- * Destructuring the Response
    , adminCreateUserResponse
    , AdminCreateUserResponse
    -- * Response Lenses
    , acursUser
    , acursResponseStatus
    ) where

import Network.AWS.CognitoIdentityProvider.Types
import Network.AWS.CognitoIdentityProvider.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the request to create a user in the specified user pool.
--
--
--
-- /See:/ 'adminCreateUser' smart constructor.
data AdminCreateUser = AdminCreateUser'
  { _acuTemporaryPassword      :: !(Maybe (Sensitive Text))
  , _acuForceAliasCreation     :: !(Maybe Bool)
  , _acuDesiredDeliveryMediums :: !(Maybe [DeliveryMediumType])
  , _acuMessageAction          :: !(Maybe MessageActionType)
  , _acuUserAttributes         :: !(Maybe [AttributeType])
  , _acuValidationData         :: !(Maybe [AttributeType])
  , _acuUserPoolId             :: !Text
  , _acuUsername               :: !(Sensitive Text)
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'AdminCreateUser' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'acuTemporaryPassword' - The user's temporary password. This password must conform to the password policy that you specified when you created the user pool. The temporary password is valid only once. To complete the Admin Create User flow, the user must enter the temporary password in the sign-in page along with a new password to be used in all future sign-ins. This parameter is not required. If you do not specify a value, Amazon Cognito generates one for you. The temporary password can only be used until the user account expiration limit that you specified when you created the user pool. To reset the account after that time limit, you must call @AdminCreateUser@ again, specifying @"RESEND"@ for the @MessageAction@ parameter.
--
-- * 'acuForceAliasCreation' - This parameter is only used if the @phone_number_verified@ or @email_verified@ attribute is set to @True@ . Otherwise, it is ignored. If this parameter is set to @True@ and the phone number or email address specified in the UserAttributes parameter already exists as an alias with a different user, the API call will migrate the alias from the previous user to the newly created user. The previous user will no longer be able to log in using that alias. If this parameter is set to @False@ , the API throws an @AliasExistsException@ error if the alias already exists. The default value is @False@ .
--
-- * 'acuDesiredDeliveryMediums' - Specify @"EMAIL"@ if email will be used to send the welcome message. Specify @"SMS"@ if the phone number will be used. The default value is @"SMS"@ . More than one value can be specified.
--
-- * 'acuMessageAction' - Set to @"RESEND"@ to resend the invitation message to a user that already exists and reset the expiration limit on the user's account. Set to @"SUPPRESS"@ to suppress sending the message. Only one value can be specified.
--
-- * 'acuUserAttributes' - An array of name-value pairs that contain user attributes and attribute values to be set for the user to be created. You can create a user without specifying any attributes other than @Username@ . However, any attributes that you specify as required (in or in the __Attributes__ tab of the console) must be supplied either by you (in your call to @AdminCreateUser@ ) or by the user (when he or she signs up in response to your welcome message). For custom attributes, you must prepend the @custom:@ prefix to the attribute name. To send a message inviting the user to sign up, you must specify the user's email address or phone number. This can be done in your call to AdminCreateUser or in the __Users__ tab of the Amazon Cognito console for managing your user pools. In your call to @AdminCreateUser@ , you can set the @email_verified@ attribute to @True@ , and you can set the @phone_number_verified@ attribute to @True@ . (You can also do this by calling .)     * __email__ : The email address of the user to whom the message that contains the code and username will be sent. Required if the @email_verified@ attribute is set to @True@ , or if @"EMAIL"@ is specified in the @DesiredDeliveryMediums@ parameter.     * __phone_number__ : The phone number of the user to whom the message that contains the code and username will be sent. Required if the @phone_number_verified@ attribute is set to @True@ , or if @"SMS"@ is specified in the @DesiredDeliveryMediums@ parameter.
--
-- * 'acuValidationData' - The user's validation data. This is an array of name-value pairs that contain user attributes and attribute values that you can use for custom validation, such as restricting the types of user accounts that can be registered. For example, you might choose to allow or disallow user sign-up based on the user's domain. To configure custom validation, you must create a Pre Sign-up Lambda trigger for the user pool as described in the Amazon Cognito Developer Guide. The Lambda trigger receives the validation data and uses it in the validation process. The user's validation data is not persisted.
--
-- * 'acuUserPoolId' - The user pool ID for the user pool where the user will be created.
--
-- * 'acuUsername' - The username for the user. Must be unique within the user pool. Must be a UTF-8 string between 1 and 128 characters. After the user is created, the username cannot be changed.
adminCreateUser
    :: Text -- ^ 'acuUserPoolId'
    -> Text -- ^ 'acuUsername'
    -> AdminCreateUser
adminCreateUser pUserPoolId_ pUsername_ =
  AdminCreateUser'
    { _acuTemporaryPassword = Nothing
    , _acuForceAliasCreation = Nothing
    , _acuDesiredDeliveryMediums = Nothing
    , _acuMessageAction = Nothing
    , _acuUserAttributes = Nothing
    , _acuValidationData = Nothing
    , _acuUserPoolId = pUserPoolId_
    , _acuUsername = _Sensitive # pUsername_
    }


-- | The user's temporary password. This password must conform to the password policy that you specified when you created the user pool. The temporary password is valid only once. To complete the Admin Create User flow, the user must enter the temporary password in the sign-in page along with a new password to be used in all future sign-ins. This parameter is not required. If you do not specify a value, Amazon Cognito generates one for you. The temporary password can only be used until the user account expiration limit that you specified when you created the user pool. To reset the account after that time limit, you must call @AdminCreateUser@ again, specifying @"RESEND"@ for the @MessageAction@ parameter.
acuTemporaryPassword :: Lens' AdminCreateUser (Maybe Text)
acuTemporaryPassword = lens _acuTemporaryPassword (\ s a -> s{_acuTemporaryPassword = a}) . mapping _Sensitive

-- | This parameter is only used if the @phone_number_verified@ or @email_verified@ attribute is set to @True@ . Otherwise, it is ignored. If this parameter is set to @True@ and the phone number or email address specified in the UserAttributes parameter already exists as an alias with a different user, the API call will migrate the alias from the previous user to the newly created user. The previous user will no longer be able to log in using that alias. If this parameter is set to @False@ , the API throws an @AliasExistsException@ error if the alias already exists. The default value is @False@ .
acuForceAliasCreation :: Lens' AdminCreateUser (Maybe Bool)
acuForceAliasCreation = lens _acuForceAliasCreation (\ s a -> s{_acuForceAliasCreation = a})

-- | Specify @"EMAIL"@ if email will be used to send the welcome message. Specify @"SMS"@ if the phone number will be used. The default value is @"SMS"@ . More than one value can be specified.
acuDesiredDeliveryMediums :: Lens' AdminCreateUser [DeliveryMediumType]
acuDesiredDeliveryMediums = lens _acuDesiredDeliveryMediums (\ s a -> s{_acuDesiredDeliveryMediums = a}) . _Default . _Coerce

-- | Set to @"RESEND"@ to resend the invitation message to a user that already exists and reset the expiration limit on the user's account. Set to @"SUPPRESS"@ to suppress sending the message. Only one value can be specified.
acuMessageAction :: Lens' AdminCreateUser (Maybe MessageActionType)
acuMessageAction = lens _acuMessageAction (\ s a -> s{_acuMessageAction = a})

-- | An array of name-value pairs that contain user attributes and attribute values to be set for the user to be created. You can create a user without specifying any attributes other than @Username@ . However, any attributes that you specify as required (in or in the __Attributes__ tab of the console) must be supplied either by you (in your call to @AdminCreateUser@ ) or by the user (when he or she signs up in response to your welcome message). For custom attributes, you must prepend the @custom:@ prefix to the attribute name. To send a message inviting the user to sign up, you must specify the user's email address or phone number. This can be done in your call to AdminCreateUser or in the __Users__ tab of the Amazon Cognito console for managing your user pools. In your call to @AdminCreateUser@ , you can set the @email_verified@ attribute to @True@ , and you can set the @phone_number_verified@ attribute to @True@ . (You can also do this by calling .)     * __email__ : The email address of the user to whom the message that contains the code and username will be sent. Required if the @email_verified@ attribute is set to @True@ , or if @"EMAIL"@ is specified in the @DesiredDeliveryMediums@ parameter.     * __phone_number__ : The phone number of the user to whom the message that contains the code and username will be sent. Required if the @phone_number_verified@ attribute is set to @True@ , or if @"SMS"@ is specified in the @DesiredDeliveryMediums@ parameter.
acuUserAttributes :: Lens' AdminCreateUser [AttributeType]
acuUserAttributes = lens _acuUserAttributes (\ s a -> s{_acuUserAttributes = a}) . _Default . _Coerce

-- | The user's validation data. This is an array of name-value pairs that contain user attributes and attribute values that you can use for custom validation, such as restricting the types of user accounts that can be registered. For example, you might choose to allow or disallow user sign-up based on the user's domain. To configure custom validation, you must create a Pre Sign-up Lambda trigger for the user pool as described in the Amazon Cognito Developer Guide. The Lambda trigger receives the validation data and uses it in the validation process. The user's validation data is not persisted.
acuValidationData :: Lens' AdminCreateUser [AttributeType]
acuValidationData = lens _acuValidationData (\ s a -> s{_acuValidationData = a}) . _Default . _Coerce

-- | The user pool ID for the user pool where the user will be created.
acuUserPoolId :: Lens' AdminCreateUser Text
acuUserPoolId = lens _acuUserPoolId (\ s a -> s{_acuUserPoolId = a})

-- | The username for the user. Must be unique within the user pool. Must be a UTF-8 string between 1 and 128 characters. After the user is created, the username cannot be changed.
acuUsername :: Lens' AdminCreateUser Text
acuUsername = lens _acuUsername (\ s a -> s{_acuUsername = a}) . _Sensitive

instance AWSRequest AdminCreateUser where
        type Rs AdminCreateUser = AdminCreateUserResponse
        request = postJSON cognitoIdentityProvider
        response
          = receiveJSON
              (\ s h x ->
                 AdminCreateUserResponse' <$>
                   (x .?> "User") <*> (pure (fromEnum s)))

instance Hashable AdminCreateUser where

instance NFData AdminCreateUser where

instance ToHeaders AdminCreateUser where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSCognitoIdentityProviderService.AdminCreateUser"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON AdminCreateUser where
        toJSON AdminCreateUser'{..}
          = object
              (catMaybes
                 [("TemporaryPassword" .=) <$> _acuTemporaryPassword,
                  ("ForceAliasCreation" .=) <$> _acuForceAliasCreation,
                  ("DesiredDeliveryMediums" .=) <$>
                    _acuDesiredDeliveryMediums,
                  ("MessageAction" .=) <$> _acuMessageAction,
                  ("UserAttributes" .=) <$> _acuUserAttributes,
                  ("ValidationData" .=) <$> _acuValidationData,
                  Just ("UserPoolId" .= _acuUserPoolId),
                  Just ("Username" .= _acuUsername)])

instance ToPath AdminCreateUser where
        toPath = const "/"

instance ToQuery AdminCreateUser where
        toQuery = const mempty

-- | Represents the response from the server to the request to create the user.
--
--
--
-- /See:/ 'adminCreateUserResponse' smart constructor.
data AdminCreateUserResponse = AdminCreateUserResponse'
  { _acursUser           :: !(Maybe UserType)
  , _acursResponseStatus :: !Int
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'AdminCreateUserResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'acursUser' - The newly created user.
--
-- * 'acursResponseStatus' - -- | The response status code.
adminCreateUserResponse
    :: Int -- ^ 'acursResponseStatus'
    -> AdminCreateUserResponse
adminCreateUserResponse pResponseStatus_ =
  AdminCreateUserResponse'
    {_acursUser = Nothing, _acursResponseStatus = pResponseStatus_}


-- | The newly created user.
acursUser :: Lens' AdminCreateUserResponse (Maybe UserType)
acursUser = lens _acursUser (\ s a -> s{_acursUser = a})

-- | -- | The response status code.
acursResponseStatus :: Lens' AdminCreateUserResponse Int
acursResponseStatus = lens _acursResponseStatus (\ s a -> s{_acursResponseStatus = a})

instance NFData AdminCreateUserResponse where
