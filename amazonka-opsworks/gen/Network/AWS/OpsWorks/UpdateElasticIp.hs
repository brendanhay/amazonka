{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.OpsWorks.UpdateElasticIp
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
module Network.AWS.OpsWorks.UpdateElasticIp
    (
    -- * Request
      UpdateElasticIp
    -- ** Request constructor
    , updateElasticIp
    -- ** Request lenses
    , ueiElasticIp
    , ueiName

    -- * Response
    , UpdateElasticIpResponse
    -- ** Response constructor
    , updateElasticIpResponse
    ) where

import Network.AWS.OpsWorks.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

data UpdateElasticIp = UpdateElasticIp
    { _ueiElasticIp :: Text
    , _ueiName :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'UpdateElasticIp' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ElasticIp ::@ @Text@
--
-- * @Name ::@ @Maybe Text@
--
updateElasticIp :: Text -- ^ 'ueiElasticIp'
                -> UpdateElasticIp
updateElasticIp p1 = UpdateElasticIp
    { _ueiElasticIp = p1
    , _ueiName = Nothing
    }

-- | The address.
ueiElasticIp :: Lens' UpdateElasticIp Text
ueiElasticIp = lens _ueiElasticIp (\s a -> s { _ueiElasticIp = a })

-- | The new name.
ueiName :: Lens' UpdateElasticIp (Maybe Text)
ueiName = lens _ueiName (\s a -> s { _ueiName = a })

instance ToPath UpdateElasticIp

instance ToQuery UpdateElasticIp

instance ToHeaders UpdateElasticIp

instance ToJSON UpdateElasticIp

data UpdateElasticIpResponse = UpdateElasticIpResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'UpdateElasticIpResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
updateElasticIpResponse :: UpdateElasticIpResponse
updateElasticIpResponse = UpdateElasticIpResponse

instance AWSRequest UpdateElasticIp where
    type Sv UpdateElasticIp = OpsWorks
    type Rs UpdateElasticIp = UpdateElasticIpResponse

    request = get
    response _ = nullaryResponse UpdateElasticIpResponse
