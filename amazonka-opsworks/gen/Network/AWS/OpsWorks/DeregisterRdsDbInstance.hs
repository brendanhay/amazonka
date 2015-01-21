{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.OpsWorks.DeregisterRdsDbInstance
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Deregisters an Amazon RDS instance.
--
-- Required Permissions: To use this action, an IAM user must have a Manage
-- permissions level for the stack, or an attached policy that explicitly grants
-- permissions. For more information on user permissions, see <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing UserPermissions>.
--
-- <http://docs.aws.amazon.com/opsworks/latest/APIReference/API_DeregisterRdsDbInstance.html>
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
import Network.AWS.Request.JSON
import Network.AWS.OpsWorks.Types
import qualified GHC.Exts

newtype DeregisterRdsDbInstance = DeregisterRdsDbInstance
    { _drdiRdsDbInstanceArn :: Text
    } deriving (Eq, Ord, Read, Show, Monoid, IsString)

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

data DeregisterRdsDbInstanceResponse = DeregisterRdsDbInstanceResponse
    deriving (Eq, Ord, Read, Show, Generic)

-- | 'DeregisterRdsDbInstanceResponse' constructor.
deregisterRdsDbInstanceResponse :: DeregisterRdsDbInstanceResponse
deregisterRdsDbInstanceResponse = DeregisterRdsDbInstanceResponse

instance ToPath DeregisterRdsDbInstance where
    toPath = const "/"

instance ToQuery DeregisterRdsDbInstance where
    toQuery = const mempty

instance ToHeaders DeregisterRdsDbInstance

instance ToJSON DeregisterRdsDbInstance where
    toJSON DeregisterRdsDbInstance{..} = object
        [ "RdsDbInstanceArn" .= _drdiRdsDbInstanceArn
        ]

instance AWSRequest DeregisterRdsDbInstance where
    type Sv DeregisterRdsDbInstance = OpsWorks
    type Rs DeregisterRdsDbInstance = DeregisterRdsDbInstanceResponse

    request  = post "DeregisterRdsDbInstance"
    response = nullResponse DeregisterRdsDbInstanceResponse
