{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Route53.V2013_04_01.DeleteHostedZone
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
module Network.AWS.Route53.V2013_04_01.DeleteHostedZone
    (
    -- * Request
      DeleteHostedZone
    -- ** Request constructor
    , deleteHostedZone
    -- ** Request lenses
    , dhzrId

    -- * Response
    , DeleteHostedZoneResponse
    -- ** Response lenses
    , dhzsChangeInfo
    ) where

import Network.AWS.Request.RestXML
import Network.AWS.Route53.V2013_04_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DeleteHostedZone' request.
deleteHostedZone :: Text -- ^ 'dhzrId'
                 -> DeleteHostedZone
deleteHostedZone p1 = DeleteHostedZone
    { _dhzrId = p1
    }

data DeleteHostedZone = DeleteHostedZone
    { _dhzrId :: Text
      -- ^ The ID of the request. Include this ID in a call to GetChange to
      -- track when the change has propagated to all Route 53 DNS servers.
    } deriving (Show, Generic)

-- | The ID of the request. Include this ID in a call to GetChange to track when
-- the change has propagated to all Route 53 DNS servers.
dhzrId
    :: Functor f
    => (Text
    -> f (Text))
    -> DeleteHostedZone
    -> f DeleteHostedZone
dhzrId f x =
    (\y -> x { _dhzrId = y })
       <$> f (_dhzrId x)
{-# INLINE dhzrId #-}

instance ToPath DeleteHostedZone where
    toPath DeleteHostedZone{..} = mconcat
        [ "/2013-04-01/hostedzone/"
        , toBS _dhzrId
        ]

instance ToQuery DeleteHostedZone

instance ToHeaders DeleteHostedZone

instance ToXML DeleteHostedZone where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "DeleteHostedZoneRequest"

data DeleteHostedZoneResponse = DeleteHostedZoneResponse
    { _dhzsChangeInfo :: ChangeInfo
      -- ^ A complex type that contains the ID, the status, and the date and
      -- time of your delete request.
    } deriving (Show, Generic)

-- | A complex type that contains the ID, the status, and the date and time of
-- your delete request.
dhzsChangeInfo
    :: Functor f
    => (ChangeInfo
    -> f (ChangeInfo))
    -> DeleteHostedZoneResponse
    -> f DeleteHostedZoneResponse
dhzsChangeInfo f x =
    (\y -> x { _dhzsChangeInfo = y })
       <$> f (_dhzsChangeInfo x)
{-# INLINE dhzsChangeInfo #-}

instance FromXML DeleteHostedZoneResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DeleteHostedZone where
    type Sv DeleteHostedZone = Route53
    type Rs DeleteHostedZone = DeleteHostedZoneResponse

    request = delete
    response _ = xmlResponse
