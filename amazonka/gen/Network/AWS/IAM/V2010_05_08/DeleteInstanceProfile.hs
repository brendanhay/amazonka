{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.V2010_05_08.DeleteInstanceProfile
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes the specified instance profile. The instance profile must not have
-- an associated role. Make sure you do not have any Amazon EC2 instances
-- running with the instance profile you are about to delete. Deleting a role
-- or instance profile that is associated with a running instance will break
-- any applications running on the instance. For more information about
-- instance profiles, go to About Instance Profiles.
-- https://iam.amazonaws.com/ ?Action=DeleteInstanceProfile
-- &InstanceProfileName=Webserver &Version=2010-05-08 &AUTHPARAMS
-- 90c18667-99f3-11e1-a4c3-27EXAMPLE804.
module Network.AWS.IAM.V2010_05_08.DeleteInstanceProfile where

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

data DeleteInstanceProfile = DeleteInstanceProfile
    { _diprInstanceProfileName :: Text
      -- ^ Name of the instance profile to delete.
    } deriving (Generic)

instance ToQuery DeleteInstanceProfile where
    toQuery = genericToQuery def

instance AWSRequest DeleteInstanceProfile where
    type Sv DeleteInstanceProfile = IAM
    type Rs DeleteInstanceProfile = DeleteInstanceProfileResponse

    request = post "DeleteInstanceProfile"
    response _ _ = return (Right DeleteInstanceProfileResponse)

data DeleteInstanceProfileResponse = DeleteInstanceProfileResponse
    deriving (Eq, Show, Generic)
