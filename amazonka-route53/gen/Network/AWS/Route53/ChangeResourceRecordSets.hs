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
-- Module      : Network.AWS.Route53.ChangeResourceRecordSets
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create, change, update, or delete authoritative DNS information on all Amazon Route 53 servers. Send a @POST@ request to:
--
--
-- @/2013-04-01/hostedzone//Amazon Route 53 hosted Zone ID/ /rrset@ resource.
--
-- The request body must include a document with a @ChangeResourceRecordSetsRequest@ element. The request body contains a list of change items, known as a change batch. Change batches are considered transactional changes. When using the Amazon Route 53 API to change resource record sets, Amazon Route 53 either makes all or none of the changes in a change batch request. This ensures that Amazon Route 53 never partially implements the intended changes to the resource record sets in a hosted zone.
--
-- For example, a change batch request that deletes the @CNAME@ record for www.example.com and creates an alias resource record set for www.example.com. Amazon Route 53 deletes the first resource record set and creates the second resource record set in a single operation. If either the @DELETE@ or the @CREATE@ action fails, then both changes (plus any other changes in the batch) fail, and the original @CNAME@ record continues to exist.
--
-- /Important:/ Due to the nature of transactional changes, you cannot delete the same resource record set more than once in a single change batch. If you attempt to delete the same change batch more than once, Amazon Route 53 returns an @InvalidChangeBatch@ error.
--
-- Use @ChangeResourceRecordsSetsRequest@ to perform the following actions:
--
--     * @CREATE@ :Creates a resource record set that has the specified values.
--
--     * @DELETE@ : Deletes an existing resource record set that has the specified values for @Name@ , @Type@ , @Set Identifier@ (for code latency, weighted, geolocation, and failover resource record sets), and @TTL@ (except alias resource record sets, for which the TTL is determined by the AWS resource you're routing queries to).
--
--     * @UPSERT@ : If a resource record set does not already exist, AWS creates it. If a resource set does exist, Amazon Route 53 updates it with the values in the request. Amazon Route 53 can update an existing resource record set only when all of the following values match: @Name@ , @Type@ , and @Set Identifier@ (for weighted, latency, geolocation, and failover resource record sets).
--
--
--
-- In response to a @ChangeResourceRecordSets@ request, the DNS data is changed on all Amazon Route 53 DNS servers. Initially, the status of a change is @PENDING@ , meaning the change has not yet propagated to all the authoritative Amazon Route 53 DNS servers. When the change is propagated to all hosts, the change returns a status of @INSYNC@ .
--
-- After sending a change request, confirm your change has propagated to all Amazon Route 53 DNS servers. Changes generally propagate to all Amazon Route 53 name servers in a few minutes. In rare circumstances, propagation can take up to 30 minutes. For more information, see 'GetChange' .
--
-- Note the following limitations on a @ChangeResourceRecordSets@ request:
--
--     * A request cannot contain more than 100 Change elements.
--
--     * A request cannot contain more than 1000 ResourceRecord elements.
--
--     * The sum of the number of characters (including spaces) in all @Value@ elements in a request cannot exceed 32,000 characters.
--
--     *     * The same resource cannot be deleted more than once in a single batch.
--
--
--
-- For more information on transactional changes, see 'ChangeResourceRecordSets' .
--
module Network.AWS.Route53.ChangeResourceRecordSets
    (
    -- * Creating a Request
      changeResourceRecordSets
    , ChangeResourceRecordSets
    -- * Request Lenses
    , crrsHostedZoneId
    , crrsChangeBatch

    -- * Destructuring the Response
    , changeResourceRecordSetsResponse
    , ChangeResourceRecordSetsResponse
    -- * Response Lenses
    , crrsrsResponseStatus
    , crrsrsChangeInfo
    ) where

import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.Route53.Types
import           Network.AWS.Route53.Types.Product

-- | A complex type that contains change information for the resource record set.
--
--
--
-- /See:/ 'changeResourceRecordSets' smart constructor.
data ChangeResourceRecordSets = ChangeResourceRecordSets'
    { _crrsHostedZoneId :: !Text
    , _crrsChangeBatch  :: !ChangeBatch
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ChangeResourceRecordSets' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crrsHostedZoneId' - The ID of the hosted zone that contains the resource record sets that you want to change.
--
-- * 'crrsChangeBatch' - A complex type that contains an optional comment and the @Changes@ element.
changeResourceRecordSets
    :: Text -- ^ 'crrsHostedZoneId'
    -> ChangeBatch -- ^ 'crrsChangeBatch'
    -> ChangeResourceRecordSets
changeResourceRecordSets pHostedZoneId_ pChangeBatch_ =
    ChangeResourceRecordSets'
    { _crrsHostedZoneId = pHostedZoneId_
    , _crrsChangeBatch = pChangeBatch_
    }

-- | The ID of the hosted zone that contains the resource record sets that you want to change.
crrsHostedZoneId :: Lens' ChangeResourceRecordSets Text
crrsHostedZoneId = lens _crrsHostedZoneId (\ s a -> s{_crrsHostedZoneId = a});

-- | A complex type that contains an optional comment and the @Changes@ element.
crrsChangeBatch :: Lens' ChangeResourceRecordSets ChangeBatch
crrsChangeBatch = lens _crrsChangeBatch (\ s a -> s{_crrsChangeBatch = a});

instance AWSRequest ChangeResourceRecordSets where
        type Rs ChangeResourceRecordSets =
             ChangeResourceRecordSetsResponse
        request = postXML route53
        response
          = receiveXML
              (\ s h x ->
                 ChangeResourceRecordSetsResponse' <$>
                   (pure (fromEnum s)) <*> (x .@ "ChangeInfo"))

instance Hashable ChangeResourceRecordSets

instance NFData ChangeResourceRecordSets

instance ToElement ChangeResourceRecordSets where
        toElement
          = mkElement
              "{https://route53.amazonaws.com/doc/2013-04-01/}ChangeResourceRecordSetsRequest"

instance ToHeaders ChangeResourceRecordSets where
        toHeaders = const mempty

instance ToPath ChangeResourceRecordSets where
        toPath ChangeResourceRecordSets'{..}
          = mconcat
              ["/2013-04-01/hostedzone/", toBS _crrsHostedZoneId,
               "/rrset/"]

instance ToQuery ChangeResourceRecordSets where
        toQuery = const mempty

instance ToXML ChangeResourceRecordSets where
        toXML ChangeResourceRecordSets'{..}
          = mconcat ["ChangeBatch" @= _crrsChangeBatch]

-- | A complex type containing the response for the request.
--
--
--
-- /See:/ 'changeResourceRecordSetsResponse' smart constructor.
data ChangeResourceRecordSetsResponse = ChangeResourceRecordSetsResponse'
    { _crrsrsResponseStatus :: !Int
    , _crrsrsChangeInfo     :: !ChangeInfo
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ChangeResourceRecordSetsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crrsrsResponseStatus' - -- | The response status code.
--
-- * 'crrsrsChangeInfo' - A complex type that contains information about changes made to your hosted zone. This element contains an ID that you use when performing a 'GetChange' action to get detailed information about the change.
changeResourceRecordSetsResponse
    :: Int -- ^ 'crrsrsResponseStatus'
    -> ChangeInfo -- ^ 'crrsrsChangeInfo'
    -> ChangeResourceRecordSetsResponse
changeResourceRecordSetsResponse pResponseStatus_ pChangeInfo_ =
    ChangeResourceRecordSetsResponse'
    { _crrsrsResponseStatus = pResponseStatus_
    , _crrsrsChangeInfo = pChangeInfo_
    }

-- | -- | The response status code.
crrsrsResponseStatus :: Lens' ChangeResourceRecordSetsResponse Int
crrsrsResponseStatus = lens _crrsrsResponseStatus (\ s a -> s{_crrsrsResponseStatus = a});

-- | A complex type that contains information about changes made to your hosted zone. This element contains an ID that you use when performing a 'GetChange' action to get detailed information about the change.
crrsrsChangeInfo :: Lens' ChangeResourceRecordSetsResponse ChangeInfo
crrsrsChangeInfo = lens _crrsrsChangeInfo (\ s a -> s{_crrsrsChangeInfo = a});

instance NFData ChangeResourceRecordSetsResponse
