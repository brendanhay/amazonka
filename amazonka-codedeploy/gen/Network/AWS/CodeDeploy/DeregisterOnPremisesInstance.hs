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

-- Module      : Network.AWS.CodeDeploy.DeregisterOnPremisesInstance
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

-- | Deregisters an on-premises instance.
--
-- <http://docs.aws.amazon.com/codedeploy/latest/APIReference/API_DeregisterOnPremisesInstance.html>
module Network.AWS.CodeDeploy.DeregisterOnPremisesInstance
    (
    -- * Request
      DeregisterOnPremisesInstance
    -- ** Request constructor
    , deregisterOnPremisesInstance
    -- ** Request lenses
    , dopiInstanceName

    -- * Response
    , DeregisterOnPremisesInstanceResponse
    -- ** Response constructor
    , deregisterOnPremisesInstanceResponse
    ) where

import Network.AWS.Data (Object)
import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.CodeDeploy.Types
import qualified GHC.Exts

newtype DeregisterOnPremisesInstance = DeregisterOnPremisesInstance
    { _dopiInstanceName :: Text
    } deriving (Eq, Ord, Read, Show, Monoid, IsString)

-- | 'DeregisterOnPremisesInstance' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dopiInstanceName' @::@ 'Text'
--
deregisterOnPremisesInstance :: Text -- ^ 'dopiInstanceName'
                             -> DeregisterOnPremisesInstance
deregisterOnPremisesInstance p1 = DeregisterOnPremisesInstance
    { _dopiInstanceName = p1
    }

-- | The name of the on-premises instance to deregister.
dopiInstanceName :: Lens' DeregisterOnPremisesInstance Text
dopiInstanceName = lens _dopiInstanceName (\s a -> s { _dopiInstanceName = a })

data DeregisterOnPremisesInstanceResponse = DeregisterOnPremisesInstanceResponse
    deriving (Eq, Ord, Read, Show, Generic)

-- | 'DeregisterOnPremisesInstanceResponse' constructor.
deregisterOnPremisesInstanceResponse :: DeregisterOnPremisesInstanceResponse
deregisterOnPremisesInstanceResponse = DeregisterOnPremisesInstanceResponse

instance ToPath DeregisterOnPremisesInstance where
    toPath = const "/"

instance ToQuery DeregisterOnPremisesInstance where
    toQuery = const mempty

instance ToHeaders DeregisterOnPremisesInstance

instance ToJSON DeregisterOnPremisesInstance where
    toJSON DeregisterOnPremisesInstance{..} = object
        [ "instanceName" .= _dopiInstanceName
        ]

instance AWSRequest DeregisterOnPremisesInstance where
    type Sv DeregisterOnPremisesInstance = CodeDeploy
    type Rs DeregisterOnPremisesInstance = DeregisterOnPremisesInstanceResponse

    request  = post "DeregisterOnPremisesInstance"
    response = nullResponse DeregisterOnPremisesInstanceResponse
