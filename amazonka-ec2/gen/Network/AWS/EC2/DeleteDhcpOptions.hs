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
    , ddoDhcpOptionsId
    , ddoDryRun

    -- * Response
    , DeleteDhcpOptionsResponse
    -- ** Response constructor
    , deleteDhcpOptionsResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types

data DeleteDhcpOptions = DeleteDhcpOptions
    { _ddoDhcpOptionsId :: Text
    , _ddoDryRun        :: Maybe Bool
    } deriving (Eq, Ord, Show, Generic)

-- | 'DeleteDhcpOptions' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddoDhcpOptionsId' @::@ 'Text'
--
-- * 'ddoDryRun' @::@ 'Maybe' 'Bool'
--
deleteDhcpOptions :: Text -- ^ 'ddoDhcpOptionsId'
                  -> DeleteDhcpOptions
deleteDhcpOptions p1 = DeleteDhcpOptions
    { _ddoDhcpOptionsId = p1
    , _ddoDryRun        = Nothing
    }

-- | The ID of the DHCP options set.
ddoDhcpOptionsId :: Lens' DeleteDhcpOptions Text
ddoDhcpOptionsId = lens _ddoDhcpOptionsId (\s a -> s { _ddoDhcpOptionsId = a })

ddoDryRun :: Lens' DeleteDhcpOptions (Maybe Bool)
ddoDryRun = lens _ddoDryRun (\s a -> s { _ddoDryRun = a })

instance ToPath DeleteDhcpOptions where
    toPath = const "/"

instance ToQuery DeleteDhcpOptions

data DeleteDhcpOptionsResponse = DeleteDhcpOptionsResponse

-- | 'DeleteDhcpOptionsResponse' constructor.
deleteDhcpOptionsResponse :: DeleteDhcpOptionsResponse
deleteDhcpOptionsResponse = DeleteDhcpOptionsResponse

instance AWSRequest DeleteDhcpOptions where
    type Sv DeleteDhcpOptions = EC2
    type Rs DeleteDhcpOptions = DeleteDhcpOptionsResponse

    request  = post "DeleteDhcpOptions"
    response = const (nullaryResponse DeleteDhcpOptionsResponse)
