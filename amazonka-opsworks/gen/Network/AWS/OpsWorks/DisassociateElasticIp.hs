{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.OpsWorks.DisassociateElasticIp
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Disassociates an Elastic IP address from its instance. The address remains
-- registered with the stack. For more information, see Resource Management.
-- Required Permissions: To use this action, an IAM user must have a Manage
-- permissions level for the stack, or an attached policy that explicitly
-- grants permissions. For more information on user permissions, see Managing
-- User Permissions.
--
-- <http://docs.aws.amazon.com/opsworks/latest/APIReference/API_DisassociateElasticIp.html>
module Network.AWS.OpsWorks.DisassociateElasticIp
    (
    -- * Request
      DisassociateElasticIp
    -- ** Request constructor
    , disassociateElasticIp
    -- ** Request lenses
    , deiElasticIp

    -- * Response
    , DisassociateElasticIpResponse
    -- ** Response constructor
    , disassociateElasticIpResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.OpsWorks.Types
import qualified GHC.Exts

newtype DisassociateElasticIp = DisassociateElasticIp
    { _deiElasticIp :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid, IsString)

-- | 'DisassociateElasticIp' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'deiElasticIp' @::@ 'Text'
--
disassociateElasticIp :: Text -- ^ 'deiElasticIp'
                      -> DisassociateElasticIp
disassociateElasticIp p1 = DisassociateElasticIp
    { _deiElasticIp = p1
    }

-- | The Elastic IP address.
deiElasticIp :: Lens' DisassociateElasticIp Text
deiElasticIp = lens _deiElasticIp (\s a -> s { _deiElasticIp = a })

data DisassociateElasticIpResponse = DisassociateElasticIpResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'DisassociateElasticIpResponse' constructor.
disassociateElasticIpResponse :: DisassociateElasticIpResponse
disassociateElasticIpResponse = DisassociateElasticIpResponse

instance ToPath DisassociateElasticIp where
    toPath = const "/"

instance ToQuery DisassociateElasticIp where
    toQuery = const mempty

instance ToHeaders DisassociateElasticIp
instance ToJSON DisassociateElasticIp where
    toJSON = genericToJSON jsonOptions

instance AWSRequest DisassociateElasticIp where
    type Sv DisassociateElasticIp = OpsWorks
    type Rs DisassociateElasticIp = DisassociateElasticIpResponse

    request  = post "DisassociateElasticIp"
    response = nullResponse DisassociateElasticIpResponse
