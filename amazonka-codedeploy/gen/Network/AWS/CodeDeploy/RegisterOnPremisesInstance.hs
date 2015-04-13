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

-- Module      : Network.AWS.CodeDeploy.RegisterOnPremisesInstance
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

-- | Registers an on-premises instance.
--
-- <http://docs.aws.amazon.com/codedeploy/latest/APIReference/API_RegisterOnPremisesInstance.html>
module Network.AWS.CodeDeploy.RegisterOnPremisesInstance
    (
    -- * Request
      RegisterOnPremisesInstance
    -- ** Request constructor
    , registerOnPremisesInstance
    -- ** Request lenses
    , ropiIamUserArn
    , ropiInstanceName

    -- * Response
    , RegisterOnPremisesInstanceResponse
    -- ** Response constructor
    , registerOnPremisesInstanceResponse
    ) where

import Network.AWS.Data (Object)
import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.CodeDeploy.Types
import qualified GHC.Exts

data RegisterOnPremisesInstance = RegisterOnPremisesInstance
    { _ropiIamUserArn   :: Text
    , _ropiInstanceName :: Text
    } deriving (Eq, Ord, Read, Show)

-- | 'RegisterOnPremisesInstance' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ropiIamUserArn' @::@ 'Text'
--
-- * 'ropiInstanceName' @::@ 'Text'
--
registerOnPremisesInstance :: Text -- ^ 'ropiInstanceName'
                           -> Text -- ^ 'ropiIamUserArn'
                           -> RegisterOnPremisesInstance
registerOnPremisesInstance p1 p2 = RegisterOnPremisesInstance
    { _ropiInstanceName = p1
    , _ropiIamUserArn   = p2
    }

-- | The ARN of the IAM user to associate with the on-premises instance.
ropiIamUserArn :: Lens' RegisterOnPremisesInstance Text
ropiIamUserArn = lens _ropiIamUserArn (\s a -> s { _ropiIamUserArn = a })

-- | The name of the on-premises instance to register.
ropiInstanceName :: Lens' RegisterOnPremisesInstance Text
ropiInstanceName = lens _ropiInstanceName (\s a -> s { _ropiInstanceName = a })

data RegisterOnPremisesInstanceResponse = RegisterOnPremisesInstanceResponse
    deriving (Eq, Ord, Read, Show, Generic)

-- | 'RegisterOnPremisesInstanceResponse' constructor.
registerOnPremisesInstanceResponse :: RegisterOnPremisesInstanceResponse
registerOnPremisesInstanceResponse = RegisterOnPremisesInstanceResponse

instance ToPath RegisterOnPremisesInstance where
    toPath = const "/"

instance ToQuery RegisterOnPremisesInstance where
    toQuery = const mempty

instance ToHeaders RegisterOnPremisesInstance

instance ToJSON RegisterOnPremisesInstance where
    toJSON RegisterOnPremisesInstance{..} = object
        [ "instanceName" .= _ropiInstanceName
        , "iamUserArn"   .= _ropiIamUserArn
        ]

instance AWSRequest RegisterOnPremisesInstance where
    type Sv RegisterOnPremisesInstance = CodeDeploy
    type Rs RegisterOnPremisesInstance = RegisterOnPremisesInstanceResponse

    request  = post "RegisterOnPremisesInstance"
    response = nullResponse RegisterOnPremisesInstanceResponse
