{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.DeleteDhcpOptions
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes the specified set of DHCP options. You must disassociate the set of
-- DHCP options before you can delete it. You can disassociate the set of DHCP
-- options by associating either a new set of options or the default set of
-- options with the VPC.
module Network.AWS.EC2.DeleteDhcpOptions
    (
    -- * Request
      DeleteDhcpOptions
    -- ** Request constructor
    , deleteDhcpOptions
    -- ** Request lenses
    , ddo1DhcpOptionsId
    , ddo1DryRun

    -- * Response
    , DeleteDhcpOptionsResponse
    -- ** Response constructor
    , deleteDhcpOptionsResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

data DeleteDhcpOptions = DeleteDhcpOptions
    { _ddo1DhcpOptionsId :: Text
    , _ddo1DryRun        :: Maybe Bool
    } deriving (Eq, Ord, Show, Generic)

-- | 'DeleteDhcpOptions' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddo1DhcpOptionsId' @::@ 'Text'
--
-- * 'ddo1DryRun' @::@ 'Maybe' 'Bool'
--
deleteDhcpOptions :: Text -- ^ 'ddo1DhcpOptionsId'
                  -> DeleteDhcpOptions
deleteDhcpOptions p1 = DeleteDhcpOptions
    { _ddo1DhcpOptionsId = p1
    , _ddo1DryRun        = Nothing
    }

-- | The ID of the DHCP options set.
ddo1DhcpOptionsId :: Lens' DeleteDhcpOptions Text
ddo1DhcpOptionsId =
    lens _ddo1DhcpOptionsId (\s a -> s { _ddo1DhcpOptionsId = a })

ddo1DryRun :: Lens' DeleteDhcpOptions (Maybe Bool)
ddo1DryRun = lens _ddo1DryRun (\s a -> s { _ddo1DryRun = a })

data DeleteDhcpOptionsResponse = DeleteDhcpOptionsResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'DeleteDhcpOptionsResponse' constructor.
deleteDhcpOptionsResponse :: DeleteDhcpOptionsResponse
deleteDhcpOptionsResponse = DeleteDhcpOptionsResponse

instance AWSRequest DeleteDhcpOptions where
    type Sv DeleteDhcpOptions = EC2
    type Rs DeleteDhcpOptions = DeleteDhcpOptionsResponse

    request  = post "DeleteDhcpOptions"
    response = nullResponse DeleteDhcpOptionsResponse

instance ToPath DeleteDhcpOptions where
    toPath = const "/"

instance ToHeaders DeleteDhcpOptions

instance ToQuery DeleteDhcpOptions
