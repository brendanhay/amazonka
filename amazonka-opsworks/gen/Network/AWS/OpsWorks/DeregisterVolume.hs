{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.OpsWorks.DeregisterVolume
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deregisters an Amazon EBS volume. The volume can then be registered by
-- another stack. For more information, see Resource Management. Required
-- Permissions: To use this action, an IAM user must have a Manage permissions
-- level for the stack, or an attached policy that explicitly grants
-- permissions. For more information on user permissions, see Managing User
-- Permissions.
--
-- <DeregisterVolume.html>
module Network.AWS.OpsWorks.DeregisterVolume
    (
    -- * Request
      DeregisterVolume
    -- ** Request constructor
    , deregisterVolume
    -- ** Request lenses
    , dvVolumeId

    -- * Response
    , DeregisterVolumeResponse
    -- ** Response constructor
    , deregisterVolumeResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.OpsWorks.Types
import qualified GHC.Exts

newtype DeregisterVolume = DeregisterVolume
    { _dvVolumeId :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid, IsString)

-- | 'DeregisterVolume' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dvVolumeId' @::@ 'Text'
--
deregisterVolume :: Text -- ^ 'dvVolumeId'
                 -> DeregisterVolume
deregisterVolume p1 = DeregisterVolume
    { _dvVolumeId = p1
    }

-- | The volume ID.
dvVolumeId :: Lens' DeregisterVolume Text
dvVolumeId = lens _dvVolumeId (\s a -> s { _dvVolumeId = a })

data DeregisterVolumeResponse = DeregisterVolumeResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'DeregisterVolumeResponse' constructor.
deregisterVolumeResponse :: DeregisterVolumeResponse
deregisterVolumeResponse = DeregisterVolumeResponse

instance AWSRequest DeregisterVolume where
    type Sv DeregisterVolume = OpsWorks
    type Rs DeregisterVolume = DeregisterVolumeResponse

    request  = post
    response = nullResponse DeregisterVolumeResponse

instance ToPath DeregisterVolume where
    toPath = const "/"

instance ToHeaders DeregisterVolume

instance ToQuery DeregisterVolume where
    toQuery = const mempty

instance ToJSON DeregisterVolume where
    toJSON = genericToJSON jsonOptions
