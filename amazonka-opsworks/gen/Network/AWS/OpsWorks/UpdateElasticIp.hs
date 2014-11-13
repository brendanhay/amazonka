{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

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

import Data.Aeson
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.OpsWorks.Types

data UpdateElasticIp = UpdateElasticIp
    { _ueiElasticIp :: Text
    , _ueiName      :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'UpdateElasticIp' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ueiElasticIp' @::@ 'Text'
--
-- * 'ueiName' @::@ 'Maybe' 'Text'
--
updateElasticIp :: Text -- ^ 'ueiElasticIp'
                -> UpdateElasticIp
updateElasticIp p1 = UpdateElasticIp
    { _ueiElasticIp = p1
    , _ueiName      = Nothing
    }

-- | The address.
ueiElasticIp :: Lens' UpdateElasticIp Text
ueiElasticIp = lens _ueiElasticIp (\s a -> s { _ueiElasticIp = a })

-- | The new name.
ueiName :: Lens' UpdateElasticIp (Maybe Text)
ueiName = lens _ueiName (\s a -> s { _ueiName = a })

instance ToPath UpdateElasticIp where
    toPath = const "/"

instance ToQuery UpdateElasticIp where
    toQuery = const mempty

instance ToHeaders UpdateElasticIp

instance ToBody UpdateElasticIp where
    toBody = toBody . encode . _ueiElasticIp

data UpdateElasticIpResponse = UpdateElasticIpResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'UpdateElasticIpResponse' constructor.
updateElasticIpResponse :: UpdateElasticIpResponse
updateElasticIpResponse = UpdateElasticIpResponse

-- FromJSON

instance AWSRequest UpdateElasticIp where
    type Sv UpdateElasticIp = OpsWorks
    type Rs UpdateElasticIp = UpdateElasticIpResponse

    request  = post'
    response = nullaryResponse UpdateElasticIpResponse
