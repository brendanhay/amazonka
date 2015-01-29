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

-- Module      : Network.AWS.OpsWorks.UpdateElasticIp
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

-- | Updates a registered Elastic IP address's name. For more information, see <http://docs.aws.amazon.com/opsworks/latest/userguide/resources.html Resource Management>.
--
-- Required Permissions: To use this action, an IAM user must have a Manage
-- permissions level for the stack, or an attached policy that explicitly grants
-- permissions. For more information on user permissions, see <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing UserPermissions>.
--
-- <http://docs.aws.amazon.com/opsworks/latest/APIReference/API_UpdateElasticIp.html>
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

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.OpsWorks.Types
import qualified GHC.Exts

data UpdateElasticIp = UpdateElasticIp
    { _ueiElasticIp :: Text
    , _ueiName      :: Maybe Text
    } deriving (Eq, Ord, Read, Show)

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

data UpdateElasticIpResponse = UpdateElasticIpResponse
    deriving (Eq, Ord, Read, Show, Generic)

-- | 'UpdateElasticIpResponse' constructor.
updateElasticIpResponse :: UpdateElasticIpResponse
updateElasticIpResponse = UpdateElasticIpResponse

instance ToPath UpdateElasticIp where
    toPath = const "/"

instance ToQuery UpdateElasticIp where
    toQuery = const mempty

instance ToHeaders UpdateElasticIp

instance ToJSON UpdateElasticIp where
    toJSON UpdateElasticIp{..} = object
        [ "ElasticIp" .= _ueiElasticIp
        , "Name"      .= _ueiName
        ]

instance AWSRequest UpdateElasticIp where
    type Sv UpdateElasticIp = OpsWorks
    type Rs UpdateElasticIp = UpdateElasticIpResponse

    request  = post "UpdateElasticIp"
    response = nullResponse UpdateElasticIpResponse
