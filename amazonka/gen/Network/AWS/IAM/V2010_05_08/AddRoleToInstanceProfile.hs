{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.V2010_05_08.AddRoleToInstanceProfile
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Adds the specified role to the specified instance profile. For more
-- information about roles, go to Working with Roles. For more information
-- about instance profiles, go to About Instance Profiles.
-- https://iam.amazonaws.com/ ?Action=AddRoleToInstanceProfile
-- &InstanceProfileName=Webserver &RoleName=S3Access &Version=2010-05-08
-- &AUTHPARAMS 12657608-99f2-11e1-a4c3-27EXAMPLE804.
module Network.AWS.IAM.V2010_05_08.AddRoleToInstanceProfile where

import           Control.Applicative
import           Data.ByteString      (ByteString)
import           Data.Default
import           Data.HashMap.Strict  (HashMap)
import           Data.Monoid
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           GHC.Generics
import           Network.AWS.Data
import           Network.AWS.Response
import           Network.AWS.Types    hiding (Error, Endpoint, Region)
import           Network.AWS.Request.Query
import           Network.AWS.IAM.V2010_05_08.Types
import           Network.HTTP.Client  (RequestBody, Response)
import           Prelude              hiding (head)

data AddRoleToInstanceProfile = AddRoleToInstanceProfile
    { _artiprInstanceProfileName :: Text
      -- ^ Name of the instance profile to update.
    , _artiprRoleName :: Text
      -- ^ Name of the role to add.
    } deriving (Generic)

instance ToQuery AddRoleToInstanceProfile where
    toQuery = genericToQuery def

instance AWSRequest AddRoleToInstanceProfile where
    type Sv AddRoleToInstanceProfile = IAM
    type Rs AddRoleToInstanceProfile = AddRoleToInstanceProfileResponse

    request = post "AddRoleToInstanceProfile"
    response _ _ = return (Right AddRoleToInstanceProfileResponse)

data AddRoleToInstanceProfileResponse = AddRoleToInstanceProfileResponse
    deriving (Eq, Show, Generic)
