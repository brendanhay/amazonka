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

-- Module      : Network.AWS.OpsWorks.AssociateElasticIp
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Associates one of the stack's registered Elastic IP addresses with a
-- specified instance. The address must first be registered with the stack by
-- calling RegisterElasticIp. For more information, see Resource Management.
-- Required Permissions: To use this action, an IAM user must have a Manage
-- permissions level for the stack, or an attached policy that explicitly
-- grants permissions. For more information on user permissions, see Managing
-- User Permissions.
module Network.AWS.OpsWorks.AssociateElasticIp
    (
    -- * Request
      AssociateElasticIp
    -- ** Request constructor
    , associateElasticIp
    -- ** Request lenses
    , aeiElasticIp
    , aeiInstanceId

    -- * Response
    , AssociateElasticIpResponse
    -- ** Response constructor
    , associateElasticIpResponse
    ) where

import Data.Aeson
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.OpsWorks.Types

data AssociateElasticIp = AssociateElasticIp
    { _aeiElasticIp  :: Text
    , _aeiInstanceId :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'AssociateElasticIp' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'aeiElasticIp' @::@ 'Text'
--
-- * 'aeiInstanceId' @::@ 'Maybe' 'Text'
--
associateElasticIp :: Text -- ^ 'aeiElasticIp'
                   -> AssociateElasticIp
associateElasticIp p1 = AssociateElasticIp
    { _aeiElasticIp  = p1
    , _aeiInstanceId = Nothing
    }

-- | The Elastic IP address.
aeiElasticIp :: Lens' AssociateElasticIp Text
aeiElasticIp = lens _aeiElasticIp (\s a -> s { _aeiElasticIp = a })

-- | The instance ID.
aeiInstanceId :: Lens' AssociateElasticIp (Maybe Text)
aeiInstanceId = lens _aeiInstanceId (\s a -> s { _aeiInstanceId = a })

instance ToPath AssociateElasticIp where
    toPath = const "/"

instance ToQuery AssociateElasticIp where
    toQuery = const mempty

instance ToHeaders AssociateElasticIp

instance ToBody AssociateElasticIp where
    toBody = toBody . encode . _aeiElasticIp

data AssociateElasticIpResponse = AssociateElasticIpResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'AssociateElasticIpResponse' constructor.
associateElasticIpResponse :: AssociateElasticIpResponse
associateElasticIpResponse = AssociateElasticIpResponse

-- FromJSON

instance AWSRequest AssociateElasticIp where
    type Sv AssociateElasticIp = OpsWorks
    type Rs AssociateElasticIp = AssociateElasticIpResponse

    request  = post'
    response = nullaryResponse AssociateElasticIpResponse
