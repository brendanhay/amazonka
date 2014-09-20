{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
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
-- options with the VPC. Example This example deletes the specified set of
-- DHCP options. https://ec2.amazonaws.com/?Action=DeleteDhcpOptions
-- &amp;DhcpOptionsId=dopt-7a8b9c2d &amp;AUTHPARAMS
-- &lt;DeleteDhcpOptionsResponse
-- xmlns="http://ec2.amazonaws.com/doc/2014-06-15/"&gt;
-- &lt;requestId&gt;7a62c49f-347e-4fc4-9331-6e8eEXAMPLE&lt;/requestId&gt;
-- &lt;return&gt;true&lt;/return&gt; &lt;/DeleteDhcpOptionsResponse&gt;.
module Network.AWS.EC2.DeleteDhcpOptions
    (
    -- * Request
      DeleteDhcpOptions
    -- ** Request constructor
    , deleteDhcpOptions
    -- ** Request lenses
    , ddoDhcpOptionsId

    -- * Response
    , DeleteDhcpOptionsResponse
    -- ** Response constructor
    , deleteDhcpOptionsResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import Network.AWS.Prelude

newtype DeleteDhcpOptions = DeleteDhcpOptions
    { _ddoDhcpOptionsId :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteDhcpOptions' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @DhcpOptionsId ::@ @Text@
--
deleteDhcpOptions :: Text -- ^ 'ddoDhcpOptionsId'
                  -> DeleteDhcpOptions
deleteDhcpOptions p1 = DeleteDhcpOptions
    { _ddoDhcpOptionsId = p1
    }

-- | The ID of the DHCP options set.
ddoDhcpOptionsId :: Lens' DeleteDhcpOptions Text
ddoDhcpOptionsId =
    lens _ddoDhcpOptionsId (\s a -> s { _ddoDhcpOptionsId = a })

instance ToQuery DeleteDhcpOptions where
    toQuery = genericQuery def

data DeleteDhcpOptionsResponse = DeleteDhcpOptionsResponse
    deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteDhcpOptionsResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
deleteDhcpOptionsResponse :: DeleteDhcpOptionsResponse
deleteDhcpOptionsResponse = DeleteDhcpOptionsResponse

instance AWSRequest DeleteDhcpOptions where
    type Sv DeleteDhcpOptions = EC2
    type Rs DeleteDhcpOptions = DeleteDhcpOptionsResponse

    request = post "DeleteDhcpOptions"
    response _ = nullaryResponse DeleteDhcpOptionsResponse
