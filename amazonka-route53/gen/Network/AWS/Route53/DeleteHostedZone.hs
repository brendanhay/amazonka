{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
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
--
-- <http://docs.aws.amazon.com/Route53/latest/APIReference/API_DeleteHostedZone.html>
module Network.AWS.Route53.DeleteHostedZone
    (
    -- * Request
      DeleteHostedZone
    -- ** Request constructor
    , deleteHostedZone
    -- ** Request lenses
    , dhzId

    -- * Response
    , DeleteHostedZoneResponse
    -- ** Response constructor
    , deleteHostedZoneResponse
    -- ** Response lenses
    , dhzrChangeInfo
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.RestXML
import Network.AWS.Route53.Types
import qualified GHC.Exts

newtype DeleteHostedZone = DeleteHostedZone
    { _dhzId :: Text
    } deriving (Eq, Ord, Show, Monoid, IsString)

-- | 'DeleteHostedZone' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dhzId' @::@ 'Text'
--
deleteHostedZone :: Text -- ^ 'dhzId'
                 -> DeleteHostedZone
deleteHostedZone p1 = DeleteHostedZone
    { _dhzId = p1
    }

-- | The ID of the hosted zone you want to delete.
dhzId :: Lens' DeleteHostedZone Text
dhzId = lens _dhzId (\s a -> s { _dhzId = a })

newtype DeleteHostedZoneResponse = DeleteHostedZoneResponse
    { _dhzrChangeInfo :: ChangeInfo
    } deriving (Eq, Show)

-- | 'DeleteHostedZoneResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dhzrChangeInfo' @::@ 'ChangeInfo'
--
deleteHostedZoneResponse :: ChangeInfo -- ^ 'dhzrChangeInfo'
                         -> DeleteHostedZoneResponse
deleteHostedZoneResponse p1 = DeleteHostedZoneResponse
    { _dhzrChangeInfo = p1
    }

-- | A complex type that contains the ID, the status, and the date and time of
-- your delete request.
dhzrChangeInfo :: Lens' DeleteHostedZoneResponse ChangeInfo
dhzrChangeInfo = lens _dhzrChangeInfo (\s a -> s { _dhzrChangeInfo = a })

instance ToPath DeleteHostedZone where
    toPath DeleteHostedZone{..} = mconcat
        [ "/2013-04-01/hostedzone/"
        , toText _dhzId
        ]

instance ToQuery DeleteHostedZone where
    toQuery = const mempty

instance ToHeaders DeleteHostedZone

instance ToXMLRoot DeleteHostedZone where
    toXMLRoot = const (namespaced ns "DeleteHostedZone" [])

instance ToXML DeleteHostedZone

instance AWSRequest DeleteHostedZone where
    type Sv DeleteHostedZone = Route53
    type Rs DeleteHostedZone = DeleteHostedZoneResponse

    request  = delete
    response = xmlResponse

instance FromXML DeleteHostedZoneResponse where
    parseXML x = DeleteHostedZoneResponse
        <$> x .@  "ChangeInfo"
