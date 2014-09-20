{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
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
module Network.AWS.OpsWorks.DisassociateElasticIp
    (
    -- * Request
      DisassociateElasticIp
    -- ** Request constructor
    , disassociateElasticIp
    -- ** Request lenses
    , dei2ElasticIp

    -- * Response
    , DisassociateElasticIpResponse
    -- ** Response constructor
    , disassociateElasticIpResponse
    ) where

import Network.AWS.OpsWorks.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

newtype DisassociateElasticIp = DisassociateElasticIp
    { _dei2ElasticIp :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DisassociateElasticIp' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ElasticIp ::@ @Text@
--
disassociateElasticIp :: Text -- ^ 'dei2ElasticIp'
                      -> DisassociateElasticIp
disassociateElasticIp p1 = DisassociateElasticIp
    { _dei2ElasticIp = p1
    }

-- | The Elastic IP address.
dei2ElasticIp :: Lens' DisassociateElasticIp Text
dei2ElasticIp = lens _dei2ElasticIp (\s a -> s { _dei2ElasticIp = a })

instance ToPath DisassociateElasticIp

instance ToQuery DisassociateElasticIp

instance ToHeaders DisassociateElasticIp

instance ToJSON DisassociateElasticIp

data DisassociateElasticIpResponse = DisassociateElasticIpResponse
    deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DisassociateElasticIpResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
disassociateElasticIpResponse :: DisassociateElasticIpResponse
disassociateElasticIpResponse = DisassociateElasticIpResponse

instance AWSRequest DisassociateElasticIp where
    type Sv DisassociateElasticIp = OpsWorks
    type Rs DisassociateElasticIp = DisassociateElasticIpResponse

    request = get
    response _ = nullaryResponse DisassociateElasticIpResponse
