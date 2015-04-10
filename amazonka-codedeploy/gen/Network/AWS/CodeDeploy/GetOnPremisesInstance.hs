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

-- Module      : Network.AWS.CodeDeploy.GetOnPremisesInstance
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

-- | Gets information about an on-premises instance.
--
-- <http://docs.aws.amazon.com/codedeploy/latest/APIReference/API_GetOnPremisesInstance.html>
module Network.AWS.CodeDeploy.GetOnPremisesInstance
    (
    -- * Request
      GetOnPremisesInstance
    -- ** Request constructor
    , getOnPremisesInstance
    -- ** Request lenses
    , gopiInstanceName

    -- * Response
    , GetOnPremisesInstanceResponse
    -- ** Response constructor
    , getOnPremisesInstanceResponse
    -- ** Response lenses
    , gopirInstanceInfo
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.CodeDeploy.Types
import qualified GHC.Exts

newtype GetOnPremisesInstance = GetOnPremisesInstance
    { _gopiInstanceName :: Text
    } deriving (Eq, Ord, Read, Show, Monoid, IsString)

-- | 'GetOnPremisesInstance' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gopiInstanceName' @::@ 'Text'
--
getOnPremisesInstance :: Text -- ^ 'gopiInstanceName'
                      -> GetOnPremisesInstance
getOnPremisesInstance p1 = GetOnPremisesInstance
    { _gopiInstanceName = p1
    }

-- | The name of the on-premises instance to get information about
gopiInstanceName :: Lens' GetOnPremisesInstance Text
gopiInstanceName = lens _gopiInstanceName (\s a -> s { _gopiInstanceName = a })

newtype GetOnPremisesInstanceResponse = GetOnPremisesInstanceResponse
    { _gopirInstanceInfo :: Maybe InstanceInfo
    } deriving (Eq, Read, Show)

-- | 'GetOnPremisesInstanceResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gopirInstanceInfo' @::@ 'Maybe' 'InstanceInfo'
--
getOnPremisesInstanceResponse :: GetOnPremisesInstanceResponse
getOnPremisesInstanceResponse = GetOnPremisesInstanceResponse
    { _gopirInstanceInfo = Nothing
    }

-- | Information about the on-premises instance.
gopirInstanceInfo :: Lens' GetOnPremisesInstanceResponse (Maybe InstanceInfo)
gopirInstanceInfo =
    lens _gopirInstanceInfo (\s a -> s { _gopirInstanceInfo = a })

instance ToPath GetOnPremisesInstance where
    toPath = const "/"

instance ToQuery GetOnPremisesInstance where
    toQuery = const mempty

instance ToHeaders GetOnPremisesInstance

instance ToJSON GetOnPremisesInstance where
    toJSON GetOnPremisesInstance{..} = object
        [ "instanceName" .= _gopiInstanceName
        ]

instance AWSRequest GetOnPremisesInstance where
    type Sv GetOnPremisesInstance = CodeDeploy
    type Rs GetOnPremisesInstance = GetOnPremisesInstanceResponse

    request  = post "GetOnPremisesInstance"
    response = jsonResponse

instance FromJSON GetOnPremisesInstanceResponse where
    parseJSON = withObject "GetOnPremisesInstanceResponse" $ \o -> GetOnPremisesInstanceResponse
        <$> o .:? "instanceInfo"
