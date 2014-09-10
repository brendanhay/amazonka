{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Route53.DeleteHostedZone
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | This action deletes a hosted zone. To delete a hosted zone, send a DELETE
-- request to the 2013-04-01/hostedzone/hosted zone ID resource. For more
-- information about deleting a hosted zone, see Deleting a Hosted Zone in the
-- Amazon Route 53 Developer Guide. You can delete a hosted zone only if there
-- are no resource record sets other than the default SOA record and NS
-- resource record sets. If your hosted zone contains other resource record
-- sets, you must delete them before you can delete your hosted zone. If you
-- try to delete a hosted zone that contains other resource record sets, Route
-- 53 will deny your request with a HostedZoneNotEmpty error. For information
-- about deleting records from your hosted zone, see ChangeResourceRecordSets.
module Network.AWS.Route53
    (
    -- * Request
      DeleteHostedZone
    -- ** Request constructor
    , mkDeleteHostedZone
    -- ** Request lenses
    , dhzId

    -- * Response
    , DeleteHostedZoneResponse
    -- ** Response constructor
    , mkDeleteHostedZoneResponse
    -- ** Response lenses
    , dhzrChangeInfo
    ) where

import Network.AWS.Request.RestXML
import Network.AWS.Route53.Types
import Network.AWS.Prelude
import Network.AWS.Types (Region)

-- | A complex type containing the response information for the delete request.
newtype DeleteHostedZone = DeleteHostedZone
    { _dhzId :: !Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteHostedZone' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Id ::@ @Text@
--
mkDeleteHostedZone :: Text -- ^ 'dhzId'
                   -> DeleteHostedZone
mkDeleteHostedZone p1 = DeleteHostedZone
    { _dhzId = p1
    }

-- | The ID of the request. Include this ID in a call to GetChange to track when
-- the change has propagated to all Route 53 DNS servers.
dhzId :: Lens' DeleteHostedZone Text
dhzId = lens _dhzId (\s a -> s { _dhzId = a })

instance ToPath DeleteHostedZone

instance ToQuery DeleteHostedZone

instance ToHeaders DeleteHostedZone

instance ToXML DeleteHostedZone where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "DeleteHostedZoneRequest"

-- | A complex type containing the response information for the request.
newtype DeleteHostedZoneResponse = DeleteHostedZoneResponse
    { _dhzrChangeInfo :: ChangeInfo
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteHostedZoneResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ChangeInfo ::@ @ChangeInfo@
--
mkDeleteHostedZoneResponse :: ChangeInfo -- ^ 'dhzrChangeInfo'
                           -> DeleteHostedZoneResponse
mkDeleteHostedZoneResponse p1 = DeleteHostedZoneResponse
    { _dhzrChangeInfo = p1
    }

-- | A complex type that contains the ID, the status, and the date and time of
-- your delete request.
dhzrChangeInfo :: Lens' DeleteHostedZoneResponse ChangeInfo
dhzrChangeInfo = lens _dhzrChangeInfo (\s a -> s { _dhzrChangeInfo = a })

instance FromXML DeleteHostedZoneResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DeleteHostedZone where
    type Sv DeleteHostedZone = Route53
    type Rs DeleteHostedZone = DeleteHostedZoneResponse

    request = get
    response _ = xmlResponse
