{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.DeleteHostedZone
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This action deletes a hosted zone. To delete a hosted zone, send a
-- 'DELETE' request to the
-- '\/Route 53 API version\/hostedzone\/hosted zone ID' resource.
--
-- For more information about deleting a hosted zone, see
-- <http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/DeleteHostedZone.html Deleting a Hosted Zone>
-- in the /Amazon Route 53 Developer Guide/.
--
-- You can delete a hosted zone only if there are no resource record sets
-- other than the default SOA record and NS resource record sets. If your
-- hosted zone contains other resource record sets, you must delete them
-- before you can delete your hosted zone. If you try to delete a hosted
-- zone that contains other resource record sets, Amazon Route 53 will deny
-- your request with a 'HostedZoneNotEmpty' error. For information about
-- deleting records from your hosted zone, see < ChangeResourceRecordSets>.
module Network.AWS.Route53.DeleteHostedZone
    (
    -- * Creating a Request
      deleteHostedZone
    , DeleteHostedZone
    -- * Request Lenses
    , dhzId

    -- * Destructuring the Response
    , deleteHostedZoneResponse
    , DeleteHostedZoneResponse
    -- * Response Lenses
    , dhzrsResponseStatus
    , dhzrsChangeInfo
    ) where

import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.Route53.Types
import           Network.AWS.Route53.Types.Product

-- | A complex type that contains information about the hosted zone that you
-- want to delete.
--
-- /See:/ 'deleteHostedZone' smart constructor.
newtype DeleteHostedZone = DeleteHostedZone'
    { _dhzId :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeleteHostedZone' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dhzId'
deleteHostedZone
    :: Text -- ^ 'dhzId'
    -> DeleteHostedZone
deleteHostedZone pId_ =
    DeleteHostedZone'
    { _dhzId = pId_
    }

-- | The ID of the hosted zone you want to delete.
dhzId :: Lens' DeleteHostedZone Text
dhzId = lens _dhzId (\ s a -> s{_dhzId = a});

instance AWSRequest DeleteHostedZone where
        type Rs DeleteHostedZone = DeleteHostedZoneResponse
        request = delete route53
        response
          = receiveXML
              (\ s h x ->
                 DeleteHostedZoneResponse' <$>
                   (pure (fromEnum s)) <*> (x .@ "ChangeInfo"))

instance Hashable DeleteHostedZone

instance ToHeaders DeleteHostedZone where
        toHeaders = const mempty

instance ToPath DeleteHostedZone where
        toPath DeleteHostedZone'{..}
          = mconcat ["/2013-04-01/hostedzone/", toBS _dhzId]

instance ToQuery DeleteHostedZone where
        toQuery = const mempty

-- | A complex type containing the response information for the request.
--
-- /See:/ 'deleteHostedZoneResponse' smart constructor.
data DeleteHostedZoneResponse = DeleteHostedZoneResponse'
    { _dhzrsResponseStatus :: !Int
    , _dhzrsChangeInfo     :: !ChangeInfo
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeleteHostedZoneResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dhzrsResponseStatus'
--
-- * 'dhzrsChangeInfo'
deleteHostedZoneResponse
    :: Int -- ^ 'dhzrsResponseStatus'
    -> ChangeInfo -- ^ 'dhzrsChangeInfo'
    -> DeleteHostedZoneResponse
deleteHostedZoneResponse pResponseStatus_ pChangeInfo_ =
    DeleteHostedZoneResponse'
    { _dhzrsResponseStatus = pResponseStatus_
    , _dhzrsChangeInfo = pChangeInfo_
    }

-- | The response status code.
dhzrsResponseStatus :: Lens' DeleteHostedZoneResponse Int
dhzrsResponseStatus = lens _dhzrsResponseStatus (\ s a -> s{_dhzrsResponseStatus = a});

-- | A complex type that contains the ID, the status, and the date and time
-- of your delete request.
dhzrsChangeInfo :: Lens' DeleteHostedZoneResponse ChangeInfo
dhzrsChangeInfo = lens _dhzrsChangeInfo (\ s a -> s{_dhzrsChangeInfo = a});
