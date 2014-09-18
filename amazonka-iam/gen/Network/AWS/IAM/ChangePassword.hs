{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.ChangePassword
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Changes the password of the IAM user calling ChangePassword. The root
-- account password is not affected by this action. For information about
-- modifying passwords, see Managing Passwords in the Using IAM guide.
-- https://iam.amazonaws.com/ ?Action=ChangePassword &OldPassword=U79}kgds4?
-- &NewPassword=Lb0*1(9xpN &Version=2010-05-08 &AUTHPARAMS
-- 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
module Network.AWS.IAM.ChangePassword
    (
    -- * Request
      ChangePassword
    -- ** Request constructor
    , changePassword
    -- ** Request lenses
    , cpOldPassword
    , cpNewPassword

    -- * Response
    , ChangePasswordResponse
    -- ** Response constructor
    , changePasswordResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.IAM.Types
import Network.AWS.Prelude

data ChangePassword = ChangePassword
    { _cpOldPassword :: Text
    , _cpNewPassword :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ChangePassword' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @OldPassword ::@ @Text@
--
-- * @NewPassword ::@ @Text@
--
changePassword :: Text -- ^ 'cpOldPassword'
                 -> Text -- ^ 'cpNewPassword'
                 -> ChangePassword
changePassword p1 p2 = ChangePassword
    { _cpOldPassword = p1
    , _cpNewPassword = p2
    }

-- | The IAM users's current password.
cpOldPassword :: Lens' ChangePassword Text
cpOldPassword = lens _cpOldPassword (\s a -> s { _cpOldPassword = a })

-- | The new password. The new password must conform to the AWS account's
-- password policy, if one exists.
cpNewPassword :: Lens' ChangePassword Text
cpNewPassword = lens _cpNewPassword (\s a -> s { _cpNewPassword = a })

instance ToQuery ChangePassword where
    toQuery = genericQuery def

data ChangePasswordResponse = ChangePasswordResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ChangePasswordResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
changePasswordResponse :: ChangePasswordResponse
changePasswordResponse = ChangePasswordResponse

instance AWSRequest ChangePassword where
    type Sv ChangePassword = IAM
    type Rs ChangePassword = ChangePasswordResponse

    request = post "ChangePassword"
    response _ = nullaryResponse ChangePasswordResponse
