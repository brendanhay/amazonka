{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.Route53.DeleteHostedZone
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | This action deletes a hosted zone. To delete a hosted zone, send a
-- @DELETE@ request to the @2013-04-01\/hostedzone\/hosted zone ID@
-- resource.
--
-- For more information about deleting a hosted zone, see
-- <http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/DeleteHostedZone.html Deleting a Hosted Zone>
-- in the /Amazon Route 53 Developer Guide/.
--
-- You can delete a hosted zone only if there are no resource record sets
-- other than the default SOA record and NS resource record sets. If your
-- hosted zone contains other resource record sets, you must delete them
-- before you can delete your hosted zone. If you try to delete a hosted
-- zone that contains other resource record sets, Route 53 will deny your
-- request with a @HostedZoneNotEmpty@ error. For information about
-- deleting records from your hosted zone, see ChangeResourceRecordSets.
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
    , dhzrStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.Route53.Types

-- | A complex type that contains information about the hosted zone that you
-- want to delete.
--
-- /See:/ 'deleteHostedZone' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dhzId'
newtype DeleteHostedZone = DeleteHostedZone'
    { _dhzId :: Text
    } deriving (Eq,Read,Show)

-- | 'DeleteHostedZone' smart constructor.
deleteHostedZone :: Text -> DeleteHostedZone
deleteHostedZone pId =
    DeleteHostedZone'
    { _dhzId = pId
    }

-- | The ID of the hosted zone you want to delete.
dhzId :: Lens' DeleteHostedZone Text
dhzId = lens _dhzId (\ s a -> s{_dhzId = a});

instance AWSRequest DeleteHostedZone where
        type Sv DeleteHostedZone = Route53
        type Rs DeleteHostedZone = DeleteHostedZoneResponse
        request = delete
        response
          = receiveXML
              (\ s h x ->
                 DeleteHostedZoneResponse' <$>
                   (x .@ "ChangeInfo") <*> (pure s))

instance ToHeaders DeleteHostedZone where
        toHeaders = const mempty

instance ToPath DeleteHostedZone where
        toPath DeleteHostedZone'{..}
          = mconcat ["/2013-04-01/hostedzone/", toText _dhzId]

instance ToQuery DeleteHostedZone where
        toQuery = const mempty

-- | A complex type containing the response information for the request.
--
-- /See:/ 'deleteHostedZoneResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dhzrChangeInfo'
--
-- * 'dhzrStatus'
data DeleteHostedZoneResponse = DeleteHostedZoneResponse'
    { _dhzrChangeInfo :: !ChangeInfo
    , _dhzrStatus     :: !Status
    } deriving (Eq,Read,Show)

-- | 'DeleteHostedZoneResponse' smart constructor.
deleteHostedZoneResponse :: ChangeInfo -> Status -> DeleteHostedZoneResponse
deleteHostedZoneResponse pChangeInfo pStatus =
    DeleteHostedZoneResponse'
    { _dhzrChangeInfo = pChangeInfo
    , _dhzrStatus = pStatus
    }

-- | A complex type that contains the ID, the status, and the date and time
-- of your delete request.
dhzrChangeInfo :: Lens' DeleteHostedZoneResponse ChangeInfo
dhzrChangeInfo = lens _dhzrChangeInfo (\ s a -> s{_dhzrChangeInfo = a});

-- | FIXME: Undocumented member.
dhzrStatus :: Lens' DeleteHostedZoneResponse Status
dhzrStatus = lens _dhzrStatus (\ s a -> s{_dhzrStatus = a});
