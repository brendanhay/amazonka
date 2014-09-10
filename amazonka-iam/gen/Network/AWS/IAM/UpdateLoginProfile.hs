{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.UpdateLoginProfile
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
module Network.AWS.IAM
    (
    -- * Request
      UpdateLoginProfile
    -- ** Request constructor
    , mkUpdateLoginProfile
    -- ** Request lenses
    , ulpUserName
    , ulpPassword
    , ulpPasswordResetRequired

    -- * Response
    , UpdateLoginProfileResponse
    -- ** Response constructor
    , mkUpdateLoginProfileResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.IAM.Types
import Network.AWS.Prelude

data UpdateLoginProfile = UpdateLoginProfile
    { _ulpUserName :: !Text
    , _ulpPassword :: !(Maybe Text)
    , _ulpPasswordResetRequired :: !(Maybe Bool)
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'UpdateLoginProfile' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @UserName ::@ @Text@
--
-- * @Password ::@ @Maybe Text@
--
-- * @PasswordResetRequired ::@ @Maybe Bool@
--
mkUpdateLoginProfile :: Text -- ^ 'ulpUserName'
                     -> UpdateLoginProfile
mkUpdateLoginProfile p1 = UpdateLoginProfile
    { _ulpUserName = p1
    , _ulpPassword = Nothing
    , _ulpPasswordResetRequired = Nothing
    }

-- | Name of the user whose password you want to update.
ulpUserName :: Lens' UpdateLoginProfile Text
ulpUserName = lens _ulpUserName (\s a -> s { _ulpUserName = a })

-- | The new password for the specified user.
ulpPassword :: Lens' UpdateLoginProfile (Maybe Text)
ulpPassword = lens _ulpPassword (\s a -> s { _ulpPassword = a })

-- | Require the specified user to set a new password on next sign-in.
ulpPasswordResetRequired :: Lens' UpdateLoginProfile (Maybe Bool)
ulpPasswordResetRequired =
    lens _ulpPasswordResetRequired
         (\s a -> s { _ulpPasswordResetRequired = a })

instance ToQuery UpdateLoginProfile where
    toQuery = genericQuery def

data UpdateLoginProfileResponse = UpdateLoginProfileResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'UpdateLoginProfileResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkUpdateLoginProfileResponse :: UpdateLoginProfileResponse
mkUpdateLoginProfileResponse = UpdateLoginProfileResponse

instance AWSRequest UpdateLoginProfile where
    type Sv UpdateLoginProfile = IAM
    type Rs UpdateLoginProfile = UpdateLoginProfileResponse

    request = post "UpdateLoginProfile"
    response _ = nullaryResponse UpdateLoginProfileResponse
