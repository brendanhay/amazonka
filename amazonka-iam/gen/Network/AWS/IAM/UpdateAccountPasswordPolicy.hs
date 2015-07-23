{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.UpdateAccountPasswordPolicy
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Updates the password policy settings for the AWS account.
--
-- This action does not support partial updates. No parameters are
-- required, but if you do not specify a parameter, that parameter\'s value
-- reverts to its default value. See the __Request Parameters__ section for
-- each parameter\'s default value.
--
-- For more information about using a password policy, see
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_ManagingPasswordPolicies.html Managing an IAM Password Policy>
-- in the /Using IAM/ guide.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_UpdateAccountPasswordPolicy.html>
module Network.AWS.IAM.UpdateAccountPasswordPolicy
    (
    -- * Request
      UpdateAccountPasswordPolicy
    -- ** Request constructor
    , updateAccountPasswordPolicy
    -- ** Request lenses
    , uapprqRequireNumbers
    , uapprqMinimumPasswordLength
    , uapprqPasswordReusePrevention
    , uapprqRequireLowercaseCharacters
    , uapprqMaxPasswordAge
    , uapprqHardExpiry
    , uapprqRequireSymbols
    , uapprqRequireUppercaseCharacters
    , uapprqAllowUsersToChangePassword

    -- * Response
    , UpdateAccountPasswordPolicyResponse
    -- ** Response constructor
    , updateAccountPasswordPolicyResponse
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'updateAccountPasswordPolicy' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'uapprqRequireNumbers'
--
-- * 'uapprqMinimumPasswordLength'
--
-- * 'uapprqPasswordReusePrevention'
--
-- * 'uapprqRequireLowercaseCharacters'
--
-- * 'uapprqMaxPasswordAge'
--
-- * 'uapprqHardExpiry'
--
-- * 'uapprqRequireSymbols'
--
-- * 'uapprqRequireUppercaseCharacters'
--
-- * 'uapprqAllowUsersToChangePassword'
data UpdateAccountPasswordPolicy = UpdateAccountPasswordPolicy'
    { _uapprqRequireNumbers             :: !(Maybe Bool)
    , _uapprqMinimumPasswordLength      :: !(Maybe Nat)
    , _uapprqPasswordReusePrevention    :: !(Maybe Nat)
    , _uapprqRequireLowercaseCharacters :: !(Maybe Bool)
    , _uapprqMaxPasswordAge             :: !(Maybe Nat)
    , _uapprqHardExpiry                 :: !(Maybe Bool)
    , _uapprqRequireSymbols             :: !(Maybe Bool)
    , _uapprqRequireUppercaseCharacters :: !(Maybe Bool)
    , _uapprqAllowUsersToChangePassword :: !(Maybe Bool)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UpdateAccountPasswordPolicy' smart constructor.
updateAccountPasswordPolicy :: UpdateAccountPasswordPolicy
updateAccountPasswordPolicy =
    UpdateAccountPasswordPolicy'
    { _uapprqRequireNumbers = Nothing
    , _uapprqMinimumPasswordLength = Nothing
    , _uapprqPasswordReusePrevention = Nothing
    , _uapprqRequireLowercaseCharacters = Nothing
    , _uapprqMaxPasswordAge = Nothing
    , _uapprqHardExpiry = Nothing
    , _uapprqRequireSymbols = Nothing
    , _uapprqRequireUppercaseCharacters = Nothing
    , _uapprqAllowUsersToChangePassword = Nothing
    }

-- | Specifies whether IAM user passwords must contain at least one numeric
-- character (0 to 9).
--
-- Default value: false
uapprqRequireNumbers :: Lens' UpdateAccountPasswordPolicy (Maybe Bool)
uapprqRequireNumbers = lens _uapprqRequireNumbers (\ s a -> s{_uapprqRequireNumbers = a});

-- | The minimum number of characters allowed in an IAM user password.
--
-- Default value: 6
uapprqMinimumPasswordLength :: Lens' UpdateAccountPasswordPolicy (Maybe Natural)
uapprqMinimumPasswordLength = lens _uapprqMinimumPasswordLength (\ s a -> s{_uapprqMinimumPasswordLength = a}) . mapping _Nat;

-- | Specifies the number of previous passwords that IAM users are prevented
-- from reusing. The default value of 0 means IAM users are not prevented
-- from reusing previous passwords.
--
-- Default value: 0
uapprqPasswordReusePrevention :: Lens' UpdateAccountPasswordPolicy (Maybe Natural)
uapprqPasswordReusePrevention = lens _uapprqPasswordReusePrevention (\ s a -> s{_uapprqPasswordReusePrevention = a}) . mapping _Nat;

-- | Specifies whether IAM user passwords must contain at least one lowercase
-- character from the ISO basic Latin alphabet (a to z).
--
-- Default value: false
uapprqRequireLowercaseCharacters :: Lens' UpdateAccountPasswordPolicy (Maybe Bool)
uapprqRequireLowercaseCharacters = lens _uapprqRequireLowercaseCharacters (\ s a -> s{_uapprqRequireLowercaseCharacters = a});

-- | The number of days that an IAM user password is valid. The default value
-- of 0 means IAM user passwords never expire.
--
-- Default value: 0
uapprqMaxPasswordAge :: Lens' UpdateAccountPasswordPolicy (Maybe Natural)
uapprqMaxPasswordAge = lens _uapprqMaxPasswordAge (\ s a -> s{_uapprqMaxPasswordAge = a}) . mapping _Nat;

-- | Prevents IAM users from setting a new password after their password has
-- expired.
--
-- Default value: false
uapprqHardExpiry :: Lens' UpdateAccountPasswordPolicy (Maybe Bool)
uapprqHardExpiry = lens _uapprqHardExpiry (\ s a -> s{_uapprqHardExpiry = a});

-- | Specifies whether IAM user passwords must contain at least one of the
-- following non-alphanumeric characters:
--
-- ! \@ # $ % ^ &amp; * ( ) _ + - = [ ] { } | \'
--
-- Default value: false
uapprqRequireSymbols :: Lens' UpdateAccountPasswordPolicy (Maybe Bool)
uapprqRequireSymbols = lens _uapprqRequireSymbols (\ s a -> s{_uapprqRequireSymbols = a});

-- | Specifies whether IAM user passwords must contain at least one uppercase
-- character from the ISO basic Latin alphabet (A to Z).
--
-- Default value: false
uapprqRequireUppercaseCharacters :: Lens' UpdateAccountPasswordPolicy (Maybe Bool)
uapprqRequireUppercaseCharacters = lens _uapprqRequireUppercaseCharacters (\ s a -> s{_uapprqRequireUppercaseCharacters = a});

-- | Allows all IAM users in your account to use the AWS Management Console
-- to change their own passwords. For more information, see
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/HowToPwdIAMUser.html Letting IAM Users Change Their Own Passwords>
-- in the /Using IAM/ guide.
--
-- Default value: false
uapprqAllowUsersToChangePassword :: Lens' UpdateAccountPasswordPolicy (Maybe Bool)
uapprqAllowUsersToChangePassword = lens _uapprqAllowUsersToChangePassword (\ s a -> s{_uapprqAllowUsersToChangePassword = a});

instance AWSRequest UpdateAccountPasswordPolicy where
        type Sv UpdateAccountPasswordPolicy = IAM
        type Rs UpdateAccountPasswordPolicy =
             UpdateAccountPasswordPolicyResponse
        request = post
        response
          = receiveNull UpdateAccountPasswordPolicyResponse'

instance ToHeaders UpdateAccountPasswordPolicy where
        toHeaders = const mempty

instance ToPath UpdateAccountPasswordPolicy where
        toPath = const "/"

instance ToQuery UpdateAccountPasswordPolicy where
        toQuery UpdateAccountPasswordPolicy'{..}
          = mconcat
              ["Action" =:
                 ("UpdateAccountPasswordPolicy" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "RequireNumbers" =: _uapprqRequireNumbers,
               "MinimumPasswordLength" =:
                 _uapprqMinimumPasswordLength,
               "PasswordReusePrevention" =:
                 _uapprqPasswordReusePrevention,
               "RequireLowercaseCharacters" =:
                 _uapprqRequireLowercaseCharacters,
               "MaxPasswordAge" =: _uapprqMaxPasswordAge,
               "HardExpiry" =: _uapprqHardExpiry,
               "RequireSymbols" =: _uapprqRequireSymbols,
               "RequireUppercaseCharacters" =:
                 _uapprqRequireUppercaseCharacters,
               "AllowUsersToChangePassword" =:
                 _uapprqAllowUsersToChangePassword]

-- | /See:/ 'updateAccountPasswordPolicyResponse' smart constructor.
data UpdateAccountPasswordPolicyResponse =
    UpdateAccountPasswordPolicyResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UpdateAccountPasswordPolicyResponse' smart constructor.
updateAccountPasswordPolicyResponse :: UpdateAccountPasswordPolicyResponse
updateAccountPasswordPolicyResponse = UpdateAccountPasswordPolicyResponse'
