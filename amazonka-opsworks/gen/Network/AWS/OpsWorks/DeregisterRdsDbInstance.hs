{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

{-# OPTIONS_GHC -w                      #-}

-- Module      : Network.AWS.OpsWorks.DeregisterRdsDbInstance
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deregisters an Amazon RDS instance.
module Network.AWS.OpsWorks.DeregisterRdsDbInstance
    (
    -- * Request
      DeregisterRdsDbInstance
    -- ** Request constructor
    , deregisterRdsDbInstance
    -- ** Request lenses
    , drdiRdsDbInstanceArn

    -- * Response
    , DeregisterRdsDbInstanceResponse
    -- ** Response constructor
    , deregisterRdsDbInstanceResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.OpsWorks.Types

newtype DeregisterRdsDbInstance = DeregisterRdsDbInstance
    { _drdiRdsDbInstanceArn :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid, IsString)

-- | 'DeregisterRdsDbInstance' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'drdiRdsDbInstanceArn' @::@ 'Text'
--
deregisterRdsDbInstance :: Text -- ^ 'drdiRdsDbInstanceArn'
                        -> DeregisterRdsDbInstance
deregisterRdsDbInstance p1 = DeregisterRdsDbInstance
    { _drdiRdsDbInstanceArn = p1
    }

-- | The Amazon RDS instance's ARN.
drdiRdsDbInstanceArn :: Lens' DeregisterRdsDbInstance Text
drdiRdsDbInstanceArn =
    lens _drdiRdsDbInstanceArn (\s a -> s { _drdiRdsDbInstanceArn = a })

instance ToPath DeregisterRdsDbInstance where
    toPath = const "/"

instance ToQuery DeregisterRdsDbInstance where
    toQuery = const mempty

instance ToHeaders DeregisterRdsDbInstance

instance ToBody DeregisterRdsDbInstance where
    toBody = toBody . encode . _drdiRdsDbInstanceArn

data DeregisterRdsDbInstanceResponse = DeregisterRdsDbInstanceResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'DeregisterRdsDbInstanceResponse' constructor.
deregisterRdsDbInstanceResponse :: DeregisterRdsDbInstanceResponse
deregisterRdsDbInstanceResponse = DeregisterRdsDbInstanceResponse

instance AWSRequest DeregisterRdsDbInstance where
    type Sv DeregisterRdsDbInstance = OpsWorks
    type Rs DeregisterRdsDbInstance = DeregisterRdsDbInstanceResponse

    request  = post
    response = nullaryResponse DeregisterRdsDbInstanceResponse
