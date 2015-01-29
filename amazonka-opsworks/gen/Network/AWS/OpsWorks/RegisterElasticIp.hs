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

-- Module      : Network.AWS.OpsWorks.RegisterElasticIp
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

-- | Registers an Elastic IP address with a specified stack. An address can be
-- registered with only one stack at a time. If the address is already
-- registered, you must first deregister it by calling 'DeregisterElasticIp'. For
-- more information, see <http://docs.aws.amazon.com/opsworks/latest/userguide/resources.html Resource Management>.
--
-- Required Permissions: To use this action, an IAM user must have a Manage
-- permissions level for the stack, or an attached policy that explicitly grants
-- permissions. For more information on user permissions, see <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing UserPermissions>.
--
-- <http://docs.aws.amazon.com/opsworks/latest/APIReference/API_RegisterElasticIp.html>
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

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.OpsWorks.Types
import qualified GHC.Exts

data RegisterElasticIp = RegisterElasticIp
    { _reiElasticIp :: Text
    , _reiStackId   :: Text
    } deriving (Eq, Ord, Read, Show)

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

newtype RegisterElasticIpResponse = RegisterElasticIpResponse
    { _reirElasticIp :: Maybe Text
    } deriving (Eq, Ord, Read, Show, Monoid)

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

instance ToPath RegisterElasticIp where
    toPath = const "/"

instance ToQuery RegisterElasticIp where
    toQuery = const mempty

instance ToHeaders RegisterElasticIp

instance ToJSON RegisterElasticIp where
    toJSON RegisterElasticIp{..} = object
        [ "ElasticIp" .= _reiElasticIp
        , "StackId"   .= _reiStackId
        ]

instance AWSRequest RegisterElasticIp where
    type Sv RegisterElasticIp = OpsWorks
    type Rs RegisterElasticIp = RegisterElasticIpResponse

    request  = post "RegisterElasticIp"
    response = jsonResponse

instance FromJSON RegisterElasticIpResponse where
    parseJSON = withObject "RegisterElasticIpResponse" $ \o -> RegisterElasticIpResponse
        <$> o .:? "ElasticIp"
