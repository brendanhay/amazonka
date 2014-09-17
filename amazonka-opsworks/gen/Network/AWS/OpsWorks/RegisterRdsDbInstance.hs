{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.OpsWorks.RegisterRdsDbInstance
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Registers an Amazon RDS instance with a stack.
module Network.AWS.OpsWorks.RegisterRdsDbInstance
    (
    -- * Request
      RegisterRdsDbInstance
    -- ** Request constructor
    , mkRegisterRdsDbInstance
    -- ** Request lenses
    , rrdiStackId
    , rrdiRdsDbInstanceArn
    , rrdiDbUser
    , rrdiDbPassword

    -- * Response
    , RegisterRdsDbInstanceResponse
    -- ** Response constructor
    , mkRegisterRdsDbInstanceResponse
    ) where

import Network.AWS.OpsWorks.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

data RegisterRdsDbInstance = RegisterRdsDbInstance
    { _rrdiStackId :: Text
    , _rrdiRdsDbInstanceArn :: Text
    , _rrdiDbUser :: Text
    , _rrdiDbPassword :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'RegisterRdsDbInstance' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @StackId ::@ @Text@
--
-- * @RdsDbInstanceArn ::@ @Text@
--
-- * @DbUser ::@ @Text@
--
-- * @DbPassword ::@ @Text@
--
mkRegisterRdsDbInstance :: Text -- ^ 'rrdiStackId'
                        -> Text -- ^ 'rrdiRdsDbInstanceArn'
                        -> Text -- ^ 'rrdiDbUser'
                        -> Text -- ^ 'rrdiDbPassword'
                        -> RegisterRdsDbInstance
mkRegisterRdsDbInstance p1 p2 p3 p4 = RegisterRdsDbInstance
    { _rrdiStackId = p1
    , _rrdiRdsDbInstanceArn = p2
    , _rrdiDbUser = p3
    , _rrdiDbPassword = p4
    }

-- | The stack ID.
rrdiStackId :: Lens' RegisterRdsDbInstance Text
rrdiStackId = lens _rrdiStackId (\s a -> s { _rrdiStackId = a })

-- | The Amazon RDS instance's ARN.
rrdiRdsDbInstanceArn :: Lens' RegisterRdsDbInstance Text
rrdiRdsDbInstanceArn =
    lens _rrdiRdsDbInstanceArn (\s a -> s { _rrdiRdsDbInstanceArn = a })

-- | The database's master user name.
rrdiDbUser :: Lens' RegisterRdsDbInstance Text
rrdiDbUser = lens _rrdiDbUser (\s a -> s { _rrdiDbUser = a })

-- | The database password.
rrdiDbPassword :: Lens' RegisterRdsDbInstance Text
rrdiDbPassword = lens _rrdiDbPassword (\s a -> s { _rrdiDbPassword = a })

instance ToPath RegisterRdsDbInstance

instance ToQuery RegisterRdsDbInstance

instance ToHeaders RegisterRdsDbInstance

instance ToJSON RegisterRdsDbInstance

data RegisterRdsDbInstanceResponse = RegisterRdsDbInstanceResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'RegisterRdsDbInstanceResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkRegisterRdsDbInstanceResponse :: RegisterRdsDbInstanceResponse
mkRegisterRdsDbInstanceResponse = RegisterRdsDbInstanceResponse

instance AWSRequest RegisterRdsDbInstance where
    type Sv RegisterRdsDbInstance = OpsWorks
    type Rs RegisterRdsDbInstance = RegisterRdsDbInstanceResponse

    request = get
    response _ = nullaryResponse RegisterRdsDbInstanceResponse
