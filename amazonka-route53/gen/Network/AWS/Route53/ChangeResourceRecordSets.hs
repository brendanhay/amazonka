{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.ChangeResourceRecordSets
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Use this action to create or change your authoritative DNS information.
-- To use this action, send a @POST@ request to the
-- @2013-04-01\/hostedzone\/hosted Zone ID\/rrset@ resource. The request
-- body must include an XML document with a
-- @ChangeResourceRecordSetsRequest@ element.
--
-- Changes are a list of change items and are considered transactional. For
-- more information on transactional changes, also known as change batches,
-- see
-- <http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/RRSchanges.html#RRSchanges_API Creating, Changing, and Deleting Resource Record Sets Using the Route 53 API>
-- in the /Amazon Route 53 Developer Guide/.
--
-- Due to the nature of transactional changes, you cannot delete the same
-- resource record set more than once in a single change batch. If you
-- attempt to delete the same change batch more than once, Route 53 returns
-- an @InvalidChangeBatch@ error.
--
-- In response to a @ChangeResourceRecordSets@ request, your DNS data is
-- changed on all Route 53 DNS servers. Initially, the status of a change
-- is @PENDING@. This means the change has not yet propagated to all the
-- authoritative Route 53 DNS servers. When the change is propagated to all
-- hosts, the change returns a status of @INSYNC@.
--
-- Note the following limitations on a @ChangeResourceRecordSets@ request:
--
-- - A request cannot contain more than 100 Change elements.
--
-- - A request cannot contain more than 1000 ResourceRecord elements.
--
-- The sum of the number of characters (including spaces) in all @Value@
-- elements in a request cannot exceed 32,000 characters.
--
-- /See:/ <http://docs.aws.amazon.com/Route53/latest/APIReference/API_ChangeResourceRecordSets.html AWS API Reference> for ChangeResourceRecordSets.
module Network.AWS.Route53.ChangeResourceRecordSets
    (
    -- * Creating a Request
      ChangeResourceRecordSets
    , changeResourceRecordSets
    -- * Request Lenses
    , crrsHostedZoneId
    , crrsChangeBatch

    -- * Destructuring the Response
    , ChangeResourceRecordSetsResponse
    , changeResourceRecordSetsResponse
    -- * Response Lenses
    , crrsrsStatus
    , crrsrsChangeInfo
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.Route53.Types

-- | A complex type that contains a change batch.
--
-- /See:/ 'changeResourceRecordSets' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'crrsHostedZoneId'
--
-- * 'crrsChangeBatch'
data ChangeResourceRecordSets = ChangeResourceRecordSets'
    { _crrsHostedZoneId :: !Text
    , _crrsChangeBatch  :: !ChangeBatch
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ChangeResourceRecordSets' smart constructor.
changeResourceRecordSets :: Text -> ChangeBatch -> ChangeResourceRecordSets
changeResourceRecordSets pHostedZoneId_ pChangeBatch_ =
    ChangeResourceRecordSets'
    { _crrsHostedZoneId = pHostedZoneId_
    , _crrsChangeBatch = pChangeBatch_
    }

-- | The ID of the hosted zone that contains the resource record sets that
-- you want to change.
crrsHostedZoneId :: Lens' ChangeResourceRecordSets Text
crrsHostedZoneId = lens _crrsHostedZoneId (\ s a -> s{_crrsHostedZoneId = a});

-- | A complex type that contains an optional comment and the @Changes@
-- element.
crrsChangeBatch :: Lens' ChangeResourceRecordSets ChangeBatch
crrsChangeBatch = lens _crrsChangeBatch (\ s a -> s{_crrsChangeBatch = a});

instance AWSRequest ChangeResourceRecordSets where
        type Sv ChangeResourceRecordSets = Route53
        type Rs ChangeResourceRecordSets =
             ChangeResourceRecordSetsResponse
        request = postXML
        response
          = receiveXML
              (\ s h x ->
                 ChangeResourceRecordSetsResponse' <$>
                   (pure (fromEnum s)) <*> (x .@ "ChangeInfo"))

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
-- /See:/ 'changeResourceRecordSetsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'crrsrsStatus'
--
-- * 'crrsrsChangeInfo'
data ChangeResourceRecordSetsResponse = ChangeResourceRecordSetsResponse'
    { _crrsrsStatus     :: !Int
    , _crrsrsChangeInfo :: !ChangeInfo
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ChangeResourceRecordSetsResponse' smart constructor.
changeResourceRecordSetsResponse :: Int -> ChangeInfo -> ChangeResourceRecordSetsResponse
changeResourceRecordSetsResponse pStatus_ pChangeInfo_ =
    ChangeResourceRecordSetsResponse'
    { _crrsrsStatus = pStatus_
    , _crrsrsChangeInfo = pChangeInfo_
    }

-- | Undocumented member.
crrsrsStatus :: Lens' ChangeResourceRecordSetsResponse Int
crrsrsStatus = lens _crrsrsStatus (\ s a -> s{_crrsrsStatus = a});

-- | A complex type that contains information about changes made to your
-- hosted zone.
--
-- This element contains an ID that you use when performing a GetChange
-- action to get detailed information about the change.
crrsrsChangeInfo :: Lens' ChangeResourceRecordSetsResponse ChangeInfo
crrsrsChangeInfo = lens _crrsrsChangeInfo (\ s a -> s{_crrsrsChangeInfo = a});
