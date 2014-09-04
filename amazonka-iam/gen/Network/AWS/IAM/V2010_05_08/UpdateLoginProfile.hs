{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.V2010_05_08.UpdateLoginProfile
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Changes the password for the specified user. https://iam.amazonaws.com/
-- ?Action=UpdateLoginProfile &UserName=Bob &Password=NewPassword &AUTHPARAMS
-- 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
module Network.AWS.IAM.V2010_05_08.UpdateLoginProfile
    (
    -- * Request
      UpdateLoginProfile
    -- ** Request constructor
    , mkUpdateLoginProfileRequest
    -- ** Request lenses
    , ulprUserName
    , ulprPassword
    , ulprPasswordResetRequired

    -- * Response
    , UpdateLoginProfileResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.IAM.V2010_05_08.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'UpdateLoginProfile' request.
mkUpdateLoginProfileRequest :: Text -- ^ 'ulprUserName'
                            -> UpdateLoginProfile
mkUpdateLoginProfileRequest p1 = UpdateLoginProfile
    { _ulprUserName = p1
    , _ulprPassword = Nothing
    , _ulprPasswordResetRequired = Nothing
    }
{-# INLINE mkUpdateLoginProfileRequest #-}

data UpdateLoginProfile = UpdateLoginProfile
    { _ulprUserName :: Text
      -- ^ Name of the user whose password you want to update.
    , _ulprPassword :: Maybe Text
      -- ^ The new password for the specified user.
    , _ulprPasswordResetRequired :: Maybe Bool
      -- ^ Require the specified user to set a new password on next sign-in.
    } deriving (Show, Generic)

-- | Name of the user whose password you want to update.
ulprUserName :: Lens' UpdateLoginProfile (Text)
ulprUserName = lens _ulprUserName (\s a -> s { _ulprUserName = a })
{-# INLINE ulprUserName #-}

-- | The new password for the specified user.
ulprPassword :: Lens' UpdateLoginProfile (Maybe Text)
ulprPassword = lens _ulprPassword (\s a -> s { _ulprPassword = a })
{-# INLINE ulprPassword #-}

-- | Require the specified user to set a new password on next sign-in.
ulprPasswordResetRequired :: Lens' UpdateLoginProfile (Maybe Bool)
ulprPasswordResetRequired = lens _ulprPasswordResetRequired (\s a -> s { _ulprPasswordResetRequired = a })
{-# INLINE ulprPasswordResetRequired #-}

instance ToQuery UpdateLoginProfile where
    toQuery = genericQuery def

data UpdateLoginProfileResponse = UpdateLoginProfileResponse
    deriving (Eq, Show, Generic)

instance AWSRequest UpdateLoginProfile where
    type Sv UpdateLoginProfile = IAM
    type Rs UpdateLoginProfile = UpdateLoginProfileResponse

    request = post "UpdateLoginProfile"
    response _ = nullaryResponse UpdateLoginProfileResponse
