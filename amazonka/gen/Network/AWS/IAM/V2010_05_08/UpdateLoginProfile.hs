{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

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
module Network.AWS.IAM.V2010_05_08.UpdateLoginProfile where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.Query
import Network.AWS.IAM.V2010_05_08.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'UpdateLoginProfile' request.
updateLoginProfile :: Text -- ^ '_ulprUserName'
                   -> UpdateLoginProfile
updateLoginProfile p1 = UpdateLoginProfile
    { _ulprUserName = p1
    , _ulprPasswordResetRequired = Nothing
    , _ulprPassword = Nothing
    }

data UpdateLoginProfile = UpdateLoginProfile
    { _ulprUserName :: Text
      -- ^ Name of the user whose password you want to update.
    , _ulprPasswordResetRequired :: Maybe Bool
      -- ^ Require the specified user to set a new password on next sign-in.
    , _ulprPassword :: Maybe Text
      -- ^ The new password for the specified user.
    } deriving (Show, Generic)

makeLenses ''UpdateLoginProfile

instance ToQuery UpdateLoginProfile where
    toQuery = genericToQuery def

data UpdateLoginProfileResponse = UpdateLoginProfileResponse
    deriving (Eq, Show, Generic)

makeLenses ''UpdateLoginProfileResponse

instance AWSRequest UpdateLoginProfile where
    type Sv UpdateLoginProfile = IAM
    type Rs UpdateLoginProfile = UpdateLoginProfileResponse

    request = post "UpdateLoginProfile"
    response _ _ = return (Right UpdateLoginProfileResponse)
