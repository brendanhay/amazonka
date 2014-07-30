{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.V2010_05_08.DeleteRole
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes the specified role. The role must not have any policies attached.
-- For more information about roles, go to Working with Roles. Make sure you
-- do not have any Amazon EC2 instances running with the role you are about to
-- delete. Deleting a role or instance profile that is associated with a
-- running instance will break any applications running on the instance.
-- https://iam.amazonaws.com/ ?Action=DeleteRole &RoleName=S3Access
-- &Version=2010-05-08 &AUTHPARAMS 913e3f37-99ed-11e1-a4c3-270EXAMPLE04.
module Network.AWS.IAM.V2010_05_08.DeleteRole where

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

data DeleteRole = DeleteRole
    { _drrRoleName :: Text
      -- ^ Name of the role to delete.
    } deriving (Generic)

instance ToQuery DeleteRole where
    toQuery = genericToQuery def

instance AWSRequest DeleteRole where
    type Sv DeleteRole = IAM
    type Rs DeleteRole = DeleteRoleResponse

    request = post "DeleteRole"
    response _ _ = return (Right DeleteRoleResponse)

data DeleteRoleResponse = DeleteRoleResponse
    deriving (Eq, Show, Generic)
