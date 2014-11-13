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

-- Module      : Network.AWS.OpsWorks.RegisterElasticIp
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Registers an Elastic IP address with a specified stack. An address can be
-- registered with only one stack at a time. If the address is already
-- registered, you must first deregister it by calling DeregisterElasticIp.
-- For more information, see Resource Management. Required Permissions: To use
-- this action, an IAM user must have a Manage permissions level for the
-- stack, or an attached policy that explicitly grants permissions. For more
-- information on user permissions, see Managing User Permissions.
module Network.AWS.OpsWorks.RegisterElasticIp
    (
    -- * Request
      RegisterElasticIp
    -- ** Request constructor
    , registerElasticIp
    -- ** Request lenses
    , reiElasticIp
    , reiStackId

    -- * Response
    , RegisterElasticIpResponse
    -- ** Response constructor
    , registerElasticIpResponse
    -- ** Response lenses
    , reirElasticIp
    ) where

import Data.Aeson
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.OpsWorks.Types

data RegisterElasticIp = RegisterElasticIp
    { _reiElasticIp :: Text
    , _reiStackId   :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'RegisterElasticIp' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'reiElasticIp' @::@ 'Text'
--
-- * 'reiStackId' @::@ 'Text'
--
registerElasticIp :: Text -- ^ 'reiElasticIp'
                  -> Text -- ^ 'reiStackId'
                  -> RegisterElasticIp
registerElasticIp p1 p2 = RegisterElasticIp
    { _reiElasticIp = p1
    , _reiStackId   = p2
    }

-- | The Elastic IP address.
reiElasticIp :: Lens' RegisterElasticIp Text
reiElasticIp = lens _reiElasticIp (\s a -> s { _reiElasticIp = a })

-- | The stack ID.
reiStackId :: Lens' RegisterElasticIp Text
reiStackId = lens _reiStackId (\s a -> s { _reiStackId = a })

instance ToPath RegisterElasticIp where
    toPath = const "/"

instance ToQuery RegisterElasticIp where
    toQuery = const mempty

instance ToHeaders RegisterElasticIp

instance ToBody RegisterElasticIp where
    toBody = toBody . encode . _reiElasticIp

newtype RegisterElasticIpResponse = RegisterElasticIpResponse
    { _reirElasticIp :: Maybe Text
    } deriving (Eq, Ord, Show, Generic, Monoid)

-- | 'RegisterElasticIpResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'reirElasticIp' @::@ 'Maybe' 'Text'
--
registerElasticIpResponse :: RegisterElasticIpResponse
registerElasticIpResponse = RegisterElasticIpResponse
    { _reirElasticIp = Nothing
    }

-- | The Elastic IP address.
reirElasticIp :: Lens' RegisterElasticIpResponse (Maybe Text)
reirElasticIp = lens _reirElasticIp (\s a -> s { _reirElasticIp = a })

-- FromJSON

instance AWSRequest RegisterElasticIp where
    type Sv RegisterElasticIp = OpsWorks
    type Rs RegisterElasticIp = RegisterElasticIpResponse

    request  = post'
    response = jsonResponse $ \h o -> RegisterElasticIpResponse
        <$> o .: "ElasticIp"
