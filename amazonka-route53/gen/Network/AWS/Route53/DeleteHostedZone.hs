{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.DeleteHostedZone
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- This action deletes a hosted zone. To delete a hosted zone, send a
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
    , dhzrqId

    -- * Response
    , DeleteHostedZoneResponse
    -- ** Response constructor
    , deleteHostedZoneResponse
    -- ** Response lenses
    , dhzrsStatus
    , dhzrsChangeInfo
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
-- * 'dhzrqId'
newtype DeleteHostedZone = DeleteHostedZone'
    { _dhzrqId :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteHostedZone' smart constructor.
deleteHostedZone :: Text -> DeleteHostedZone
deleteHostedZone pId =
    DeleteHostedZone'
    { _dhzrqId = pId
    }

-- | The ID of the hosted zone you want to delete.
dhzrqId :: Lens' DeleteHostedZone Text
dhzrqId = lens _dhzrqId (\ s a -> s{_dhzrqId = a});

instance AWSRequest DeleteHostedZone where
        type Sv DeleteHostedZone = Route53
        type Rs DeleteHostedZone = DeleteHostedZoneResponse
        request = delete
        response
          = receiveXML
              (\ s h x ->
                 DeleteHostedZoneResponse' <$>
                   (pure (fromEnum s)) <*> (x .@ "ChangeInfo"))

instance ToHeaders DeleteHostedZone where
        toHeaders = const mempty

instance ToPath DeleteHostedZone where
        toPath DeleteHostedZone'{..}
          = mconcat
              ["/2013-04-01/hostedzone/", toText _dhzrqId]

instance ToQuery DeleteHostedZone where
        toQuery = const mempty

-- | A complex type containing the response information for the request.
--
-- /See:/ 'deleteHostedZoneResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dhzrsStatus'
--
-- * 'dhzrsChangeInfo'
data DeleteHostedZoneResponse = DeleteHostedZoneResponse'
    { _dhzrsStatus     :: !Int
    , _dhzrsChangeInfo :: !ChangeInfo
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteHostedZoneResponse' smart constructor.
deleteHostedZoneResponse :: Int -> ChangeInfo -> DeleteHostedZoneResponse
deleteHostedZoneResponse pStatus pChangeInfo =
    DeleteHostedZoneResponse'
    { _dhzrsStatus = pStatus
    , _dhzrsChangeInfo = pChangeInfo
    }

-- | FIXME: Undocumented member.
dhzrsStatus :: Lens' DeleteHostedZoneResponse Int
dhzrsStatus = lens _dhzrsStatus (\ s a -> s{_dhzrsStatus = a});

-- | A complex type that contains the ID, the status, and the date and time
-- of your delete request.
dhzrsChangeInfo :: Lens' DeleteHostedZoneResponse ChangeInfo
dhzrsChangeInfo = lens _dhzrsChangeInfo (\ s a -> s{_dhzrsChangeInfo = a});
