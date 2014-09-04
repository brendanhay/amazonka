{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
module Network.AWS.OpsWorks.V2013_02_18.UpdateVolume
    (
    -- * Request
      UpdateVolume
    -- ** Request constructor
    , mkUpdateVolumeRequest
    -- ** Request lenses
    , uvsVolumeId
    , uvsName
    , uvsMountPoint

    -- * Response
    , UpdateVolumeResponse
    ) where

import           Network.AWS.OpsWorks.V2013_02_18.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'UpdateVolume' request.
mkUpdateVolumeRequest :: Text -- ^ 'uvsVolumeId'
                      -> UpdateVolume
mkUpdateVolumeRequest p1 = UpdateVolume
    { _uvsVolumeId = p1
    , _uvsName = Nothing
    , _uvsMountPoint = Nothing
    }
{-# INLINE mkUpdateVolumeRequest #-}

data UpdateVolume = UpdateVolume
    { _uvsVolumeId :: Text
      -- ^ The volume ID.
    , _uvsName :: Maybe Text
      -- ^ The new name.
    , _uvsMountPoint :: Maybe Text
      -- ^ The new mount point.
    } deriving (Show, Generic)

-- | The volume ID.
uvsVolumeId :: Lens' UpdateVolume (Text)
uvsVolumeId = lens _uvsVolumeId (\s a -> s { _uvsVolumeId = a })
{-# INLINE uvsVolumeId #-}

-- | The new name.
uvsName :: Lens' UpdateVolume (Maybe Text)
uvsName = lens _uvsName (\s a -> s { _uvsName = a })
{-# INLINE uvsName #-}

-- | The new mount point.
uvsMountPoint :: Lens' UpdateVolume (Maybe Text)
uvsMountPoint = lens _uvsMountPoint (\s a -> s { _uvsMountPoint = a })
{-# INLINE uvsMountPoint #-}

instance ToPath UpdateVolume

instance ToQuery UpdateVolume

instance ToHeaders UpdateVolume

instance ToJSON UpdateVolume

data UpdateVolumeResponse = UpdateVolumeResponse
    deriving (Eq, Show, Generic)

instance AWSRequest UpdateVolume where
    type Sv UpdateVolume = OpsWorks
    type Rs UpdateVolume = UpdateVolumeResponse

    request = get
    response _ = nullaryResponse UpdateVolumeResponse
