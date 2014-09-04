{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.V2010_05_08.ChangePassword
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
module Network.AWS.IAM.V2010_05_08.ChangePassword
    (
    -- * Request
      ChangePassword
    -- ** Request constructor
    , mkChangePasswordRequest
    -- ** Request lenses
    , cprOldPassword
    , cprNewPassword

    -- * Response
    , ChangePasswordResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.IAM.V2010_05_08.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ChangePassword' request.
mkChangePasswordRequest :: Text -- ^ 'cprOldPassword'
                        -> Text -- ^ 'cprNewPassword'
                        -> ChangePassword
mkChangePasswordRequest p1 p2 = ChangePassword
    { _cprOldPassword = p1
    , _cprNewPassword = p2
    }
{-# INLINE mkChangePasswordRequest #-}

data ChangePassword = ChangePassword
    { _cprOldPassword :: Text
      -- ^ The IAM users's current password.
    , _cprNewPassword :: Text
      -- ^ The new password. The new password must conform to the AWS
      -- account's password policy, if one exists.
    } deriving (Show, Generic)

-- | The IAM users's current password.
cprOldPassword :: Lens' ChangePassword (Text)
cprOldPassword = lens _cprOldPassword (\s a -> s { _cprOldPassword = a })
{-# INLINE cprOldPassword #-}

-- | The new password. The new password must conform to the AWS account's
-- password policy, if one exists.
cprNewPassword :: Lens' ChangePassword (Text)
cprNewPassword = lens _cprNewPassword (\s a -> s { _cprNewPassword = a })
{-# INLINE cprNewPassword #-}

instance ToQuery ChangePassword where
    toQuery = genericQuery def

data ChangePasswordResponse = ChangePasswordResponse
    deriving (Eq, Show, Generic)

instance AWSRequest ChangePassword where
    type Sv ChangePassword = IAM
    type Rs ChangePassword = ChangePasswordResponse

    request = post "ChangePassword"
    response _ = nullaryResponse ChangePasswordResponse
