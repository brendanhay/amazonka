{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.OpsWorks.V2013_02_18.UpdateElasticIp
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Updates a registered Elastic IP address's name. For more information, see
-- Resource Management. Required Permissions: To use this action, an IAM user
-- must have a Manage permissions level for the stack, or an attached policy
-- that explicitly grants permissions. For more information on user
-- permissions, see Managing User Permissions.
module Network.AWS.OpsWorks.V2013_02_18.UpdateElasticIp
    (
    -- * Request
      UpdateElasticIp
    -- ** Request constructor
    , mkUpdateElasticIpRequest
    -- ** Request lenses
    , ueirElasticIp
    , ueirName

    -- * Response
    , UpdateElasticIpResponse
    ) where

import           Network.AWS.OpsWorks.V2013_02_18.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'UpdateElasticIp' request.
mkUpdateElasticIpRequest :: Text -- ^ 'ueirElasticIp'
                         -> UpdateElasticIp
mkUpdateElasticIpRequest p1 = UpdateElasticIp
    { _ueirElasticIp = p1
    , _ueirName = Nothing
    }
{-# INLINE mkUpdateElasticIpRequest #-}

data UpdateElasticIp = UpdateElasticIp
    { _ueirElasticIp :: Text
      -- ^ The address.
    , _ueirName :: Maybe Text
      -- ^ The new name.
    } deriving (Show, Generic)

-- | The address.
ueirElasticIp :: Lens' UpdateElasticIp (Text)
ueirElasticIp = lens _ueirElasticIp (\s a -> s { _ueirElasticIp = a })
{-# INLINE ueirElasticIp #-}

-- | The new name.
ueirName :: Lens' UpdateElasticIp (Maybe Text)
ueirName = lens _ueirName (\s a -> s { _ueirName = a })
{-# INLINE ueirName #-}

instance ToPath UpdateElasticIp

instance ToQuery UpdateElasticIp

instance ToHeaders UpdateElasticIp

instance ToJSON UpdateElasticIp

data UpdateElasticIpResponse = UpdateElasticIpResponse
    deriving (Eq, Show, Generic)

instance AWSRequest UpdateElasticIp where
    type Sv UpdateElasticIp = OpsWorks
    type Rs UpdateElasticIp = UpdateElasticIpResponse

    request = get
    response _ = nullaryResponse UpdateElasticIpResponse
