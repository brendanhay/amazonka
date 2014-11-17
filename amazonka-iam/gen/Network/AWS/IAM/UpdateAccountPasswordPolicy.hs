{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.UpdateAccountPasswordPolicy
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Updates the password policy settings for the AWS account. For more
-- information about using a password policy, see Managing an IAM Password
-- Policy in the Using IAM guide.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_UpdateAccountPasswordPolicy.html>
module Network.AWS.IAM.UpdateAccountPasswordPolicy
    (
    -- * Request
      UpdateAccountPasswordPolicy
    -- ** Request constructor
    , updateAccountPasswordPolicy
    -- ** Request lenses
    , uappAllowUsersToChangePassword
    , uappHardExpiry
    , uappMaxPasswordAge
    , uappMinimumPasswordLength
    , uappPasswordReusePrevention
    , uappRequireLowercaseCharacters
    , uappRequireNumbers
    , uappRequireSymbols
    , uappRequireUppercaseCharacters

    -- * Response
    , UpdateAccountPasswordPolicyResponse
    -- ** Response constructor
    , updateAccountPasswordPolicyResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.IAM.Types
import qualified GHC.Exts

data UpdateAccountPasswordPolicy = UpdateAccountPasswordPolicy
    { _uappAllowUsersToChangePassword :: Maybe Bool
    , _uappHardExpiry                 :: Maybe Bool
    , _uappMaxPasswordAge             :: Maybe Nat
    , _uappMinimumPasswordLength      :: Maybe Nat
    , _uappPasswordReusePrevention    :: Maybe Nat
    , _uappRequireLowercaseCharacters :: Maybe Bool
    , _uappRequireNumbers             :: Maybe Bool
    , _uappRequireSymbols             :: Maybe Bool
    , _uappRequireUppercaseCharacters :: Maybe Bool
    } deriving (Eq, Ord, Show, Generic)

-- | 'UpdateAccountPasswordPolicy' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'uappAllowUsersToChangePassword' @::@ 'Maybe' 'Bool'
--
-- * 'uappHardExpiry' @::@ 'Maybe' 'Bool'
--
-- * 'uappMaxPasswordAge' @::@ 'Maybe' 'Natural'
--
-- * 'uappMinimumPasswordLength' @::@ 'Maybe' 'Natural'
--
-- * 'uappPasswordReusePrevention' @::@ 'Maybe' 'Natural'
--
-- * 'uappRequireLowercaseCharacters' @::@ 'Maybe' 'Bool'
--
-- * 'uappRequireNumbers' @::@ 'Maybe' 'Bool'
--
-- * 'uappRequireSymbols' @::@ 'Maybe' 'Bool'
--
-- * 'uappRequireUppercaseCharacters' @::@ 'Maybe' 'Bool'
--
updateAccountPasswordPolicy :: UpdateAccountPasswordPolicy
updateAccountPasswordPolicy = UpdateAccountPasswordPolicy
    { _uappMinimumPasswordLength      = Nothing
    , _uappRequireSymbols             = Nothing
    , _uappRequireNumbers             = Nothing
    , _uappRequireUppercaseCharacters = Nothing
    , _uappRequireLowercaseCharacters = Nothing
    , _uappAllowUsersToChangePassword = Nothing
    , _uappMaxPasswordAge             = Nothing
    , _uappPasswordReusePrevention    = Nothing
    , _uappHardExpiry                 = Nothing
    }

-- | Allows all IAM users in your account to use the AWS Management Console to
-- change their own passwords. For more information, see Letting IAM Users
-- Change Their Own Passwords in the Using IAM guide. Default value: false.
uappAllowUsersToChangePassword :: Lens' UpdateAccountPasswordPolicy (Maybe Bool)
uappAllowUsersToChangePassword =
    lens _uappAllowUsersToChangePassword
        (\s a -> s { _uappAllowUsersToChangePassword = a })

-- | Prevents IAM users from setting a new password after their password has
-- expired. Default value: false.
uappHardExpiry :: Lens' UpdateAccountPasswordPolicy (Maybe Bool)
uappHardExpiry = lens _uappHardExpiry (\s a -> s { _uappHardExpiry = a })

-- | The number of days that an IAM user password is valid. The default value
-- of 0 means IAM user passwords never expire. Default value: 0.
uappMaxPasswordAge :: Lens' UpdateAccountPasswordPolicy (Maybe Natural)
uappMaxPasswordAge =
    lens _uappMaxPasswordAge (\s a -> s { _uappMaxPasswordAge = a })
        . mapping _Nat

-- | The minimum number of characters allowed in an IAM user password. Default
-- value: 6.
uappMinimumPasswordLength :: Lens' UpdateAccountPasswordPolicy (Maybe Natural)
uappMinimumPasswordLength =
    lens _uappMinimumPasswordLength
        (\s a -> s { _uappMinimumPasswordLength = a })
            . mapping _Nat

-- | Specifies the number of previous passwords that IAM users are prevented
-- from reusing. The default value of 0 means IAM users are not prevented
-- from reusing previous passwords. Default value: 0.
uappPasswordReusePrevention :: Lens' UpdateAccountPasswordPolicy (Maybe Natural)
uappPasswordReusePrevention =
    lens _uappPasswordReusePrevention
        (\s a -> s { _uappPasswordReusePrevention = a })
            . mapping _Nat

-- | Specifies whether IAM user passwords must contain at least one lowercase
-- character from the ISO basic Latin alphabet (a to z). Default value:
-- false.
uappRequireLowercaseCharacters :: Lens' UpdateAccountPasswordPolicy (Maybe Bool)
uappRequireLowercaseCharacters =
    lens _uappRequireLowercaseCharacters
        (\s a -> s { _uappRequireLowercaseCharacters = a })

-- | Specifies whether IAM user passwords must contain at least one numeric
-- character (0 to 9). Default value: false.
uappRequireNumbers :: Lens' UpdateAccountPasswordPolicy (Maybe Bool)
uappRequireNumbers =
    lens _uappRequireNumbers (\s a -> s { _uappRequireNumbers = a })

-- | Specifies whether IAM user passwords must contain at least one of the
-- following non-alphanumeric characters: ! @ # $ % ^ &amp;amp; * ( ) _ + -
-- = [ ] { } | ' Default value: false.
uappRequireSymbols :: Lens' UpdateAccountPasswordPolicy (Maybe Bool)
uappRequireSymbols =
    lens _uappRequireSymbols (\s a -> s { _uappRequireSymbols = a })

-- | Specifies whether IAM user passwords must contain at least one uppercase
-- character from the ISO basic Latin alphabet (A to Z). Default value:
-- false.
uappRequireUppercaseCharacters :: Lens' UpdateAccountPasswordPolicy (Maybe Bool)
uappRequireUppercaseCharacters =
    lens _uappRequireUppercaseCharacters
        (\s a -> s { _uappRequireUppercaseCharacters = a })

data UpdateAccountPasswordPolicyResponse = UpdateAccountPasswordPolicyResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'UpdateAccountPasswordPolicyResponse' constructor.
updateAccountPasswordPolicyResponse :: UpdateAccountPasswordPolicyResponse
updateAccountPasswordPolicyResponse = UpdateAccountPasswordPolicyResponse

instance AWSRequest UpdateAccountPasswordPolicy where
    type Sv UpdateAccountPasswordPolicy = IAM
    type Rs UpdateAccountPasswordPolicy = UpdateAccountPasswordPolicyResponse

    request  = post "UpdateAccountPasswordPolicy"
    response = nullResponse UpdateAccountPasswordPolicyResponse

instance ToPath UpdateAccountPasswordPolicy where
    toPath = const "/"

instance ToHeaders UpdateAccountPasswordPolicy

instance ToQuery UpdateAccountPasswordPolicy
