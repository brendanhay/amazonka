{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}

{-# OPTIONS_GHC -fno-warn-unused-binds #-}

-- Module      : Network.AWS.OpsWorks.V2013_02_18.UpdateVolume
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Updates an Amazon EBS volume's name or mount point. For more information,
-- see Resource Management. Required Permissions: To use this action, an IAM
-- user must have a Manage permissions level for the stack, or an attached
-- policy that explicitly grants permissions. For more information on user
-- permissions, see Managing User Permissions.
module Network.AWS.OpsWorks.V2013_02_18.UpdateVolume where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.JSON
import Network.AWS.OpsWorks.V2013_02_18.Types
import Network.AWS.Prelude
import qualified Network.AWS.Types.Map as Map

-- | Minimum specification for a 'UpdateVolume' request.
updateVolume :: Text -- ^ '_uvsVolumeId'
             -> UpdateVolume
updateVolume p1 = UpdateVolume
    { _uvsVolumeId = p1
    , _uvsName = Nothing
    , _uvsMountPoint = Nothing
    }

data UpdateVolume = UpdateVolume
    { _uvsVolumeId :: Text
      -- ^ The volume ID.
    , _uvsName :: Maybe Text
      -- ^ The new name.
    , _uvsMountPoint :: Maybe Text
      -- ^ The new mount point.
    } deriving (Show, Generic)

makeLenses ''UpdateVolume

instance ToPath UpdateVolume

instance ToQuery UpdateVolume

instance ToHeaders UpdateVolume

instance ToJSON UpdateVolume

data UpdateVolumeResponse = UpdateVolumeResponse
    deriving (Eq, Show, Generic)

makeLenses ''UpdateVolumeResponse

instance AWSRequest UpdateVolume where
    type Sv UpdateVolume = OpsWorks
    type Rs UpdateVolume = UpdateVolumeResponse

    request = get
    response _ = nullaryResponse UpdateVolumeResponse
