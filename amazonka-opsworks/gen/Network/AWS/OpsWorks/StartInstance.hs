{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.OpsWorks.StartInstance
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Starts a specified instance. For more information, see Starting, Stopping,
-- and Rebooting Instances. Required Permissions: To use this action, an IAM
-- user must have a Manage permissions level for the stack, or an attached
-- policy that explicitly grants permissions. For more information on user
-- permissions, see Managing User Permissions.
module Network.AWS.OpsWorks.StartInstance
    (
    -- * Request
      StartInstance
    -- ** Request constructor
    , startInstance
    -- ** Request lenses
    , siInstanceId

    -- * Response
    , StartInstanceResponse
    -- ** Response constructor
    , startInstanceResponse
    ) where

import Network.AWS.OpsWorks.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

newtype StartInstance = StartInstance
    { _siInstanceId :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'StartInstance' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @InstanceId ::@ @Text@
--
startInstance :: Text -- ^ 'siInstanceId'
              -> StartInstance
startInstance p1 = StartInstance
    { _siInstanceId = p1
    }

-- | The instance ID.
siInstanceId :: Lens' StartInstance Text
siInstanceId = lens _siInstanceId (\s a -> s { _siInstanceId = a })

instance ToPath StartInstance

instance ToQuery StartInstance

instance ToHeaders StartInstance

instance ToJSON StartInstance

data StartInstanceResponse = StartInstanceResponse
    deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'StartInstanceResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
startInstanceResponse :: StartInstanceResponse
startInstanceResponse = StartInstanceResponse

instance AWSRequest StartInstance where
    type Sv StartInstance = OpsWorks
    type Rs StartInstance = StartInstanceResponse

    request = get
    response _ = nullaryResponse StartInstanceResponse
