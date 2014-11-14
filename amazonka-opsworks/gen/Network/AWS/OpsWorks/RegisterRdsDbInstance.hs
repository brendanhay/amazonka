{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

{-# OPTIONS_GHC -w                      #-}

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
    , registerRdsDbInstance
    -- ** Request lenses
    , rrdiDbPassword
    , rrdiDbUser
    , rrdiRdsDbInstanceArn
    , rrdiStackId

    -- * Response
    , RegisterRdsDbInstanceResponse
    -- ** Response constructor
    , registerRdsDbInstanceResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.OpsWorks.Types

data RegisterRdsDbInstance = RegisterRdsDbInstance
    { _rrdiDbPassword       :: Text
    , _rrdiDbUser           :: Text
    , _rrdiRdsDbInstanceArn :: Text
    , _rrdiStackId          :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'RegisterRdsDbInstance' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rrdiDbPassword' @::@ 'Text'
--
-- * 'rrdiDbUser' @::@ 'Text'
--
-- * 'rrdiRdsDbInstanceArn' @::@ 'Text'
--
-- * 'rrdiStackId' @::@ 'Text'
--
registerRdsDbInstance :: Text -- ^ 'rrdiStackId'
                      -> Text -- ^ 'rrdiRdsDbInstanceArn'
                      -> Text -- ^ 'rrdiDbUser'
                      -> Text -- ^ 'rrdiDbPassword'
                      -> RegisterRdsDbInstance
registerRdsDbInstance p1 p2 p3 p4 = RegisterRdsDbInstance
    { _rrdiStackId          = p1
    , _rrdiRdsDbInstanceArn = p2
    , _rrdiDbUser           = p3
    , _rrdiDbPassword       = p4
    }

-- | The database password.
rrdiDbPassword :: Lens' RegisterRdsDbInstance Text
rrdiDbPassword = lens _rrdiDbPassword (\s a -> s { _rrdiDbPassword = a })

-- | The database's master user name.
rrdiDbUser :: Lens' RegisterRdsDbInstance Text
rrdiDbUser = lens _rrdiDbUser (\s a -> s { _rrdiDbUser = a })

-- | The Amazon RDS instance's ARN.
rrdiRdsDbInstanceArn :: Lens' RegisterRdsDbInstance Text
rrdiRdsDbInstanceArn =
    lens _rrdiRdsDbInstanceArn (\s a -> s { _rrdiRdsDbInstanceArn = a })

-- | The stack ID.
rrdiStackId :: Lens' RegisterRdsDbInstance Text
rrdiStackId = lens _rrdiStackId (\s a -> s { _rrdiStackId = a })

instance ToPath RegisterRdsDbInstance where
    toPath = const "/"

instance ToQuery RegisterRdsDbInstance where
    toQuery = const mempty

instance ToHeaders RegisterRdsDbInstance

instance ToBody RegisterRdsDbInstance where
    toBody = toBody . encode . _rrdiStackId

data RegisterRdsDbInstanceResponse = RegisterRdsDbInstanceResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'RegisterRdsDbInstanceResponse' constructor.
registerRdsDbInstanceResponse :: RegisterRdsDbInstanceResponse
registerRdsDbInstanceResponse = RegisterRdsDbInstanceResponse

instance AWSRequest RegisterRdsDbInstance where
    type Sv RegisterRdsDbInstance = OpsWorks
    type Rs RegisterRdsDbInstance = RegisterRdsDbInstanceResponse

    request  = post
    response = nullaryResponse RegisterRdsDbInstanceResponse
