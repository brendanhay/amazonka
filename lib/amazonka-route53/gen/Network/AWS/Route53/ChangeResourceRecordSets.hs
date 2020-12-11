{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.ChangeResourceRecordSets
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates, changes, or deletes a resource record set, which contains authoritative DNS information for a specified domain name or subdomain name. For example, you can use @ChangeResourceRecordSets@ to create a resource record set that routes traffic for test.example.com to a web server that has an IP address of 192.0.2.44.
--
-- __Deleting Resource Record Sets__
-- To delete a resource record set, you must specify all the same values that you specified when you created it.
-- __Change Batches and Transactional Changes__
-- The request body must include a document with a @ChangeResourceRecordSetsRequest@ element. The request body contains a list of change items, known as a change batch. Change batches are considered transactional changes. Route 53 validates the changes in the request and then either makes all or none of the changes in the change batch request. This ensures that DNS routing isn't adversely affected by partial changes to the resource record sets in a hosted zone.
-- For example, suppose a change batch request contains two changes: it deletes the @CNAME@ resource record set for www.example.com and creates an alias resource record set for www.example.com. If validation for both records succeeds, Route 53 deletes the first resource record set and creates the second resource record set in a single operation. If validation for either the @DELETE@ or the @CREATE@ action fails, then the request is canceled, and the original @CNAME@ record continues to exist.
-- __Traffic Flow__
-- To create resource record sets for complex routing configurations, use either the traffic flow visual editor in the Route 53 console or the API actions for traffic policies and traffic policy instances. Save the configuration as a traffic policy, then associate the traffic policy with one or more domain names (such as example.com) or subdomain names (such as www.example.com), in the same hosted zone or in multiple hosted zones. You can roll back the updates if the new configuration isn't performing as expected. For more information, see <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/traffic-flow.html Using Traffic Flow to Route DNS Traffic> in the /Amazon Route 53 Developer Guide/ .
-- __Create, Delete, and Upsert__
-- Use @ChangeResourceRecordsSetsRequest@ to perform the following actions:
--
--     * @CREATE@ : Creates a resource record set that has the specified values.
--
--
--     * @DELETE@ : Deletes an existing resource record set that has the specified values.
--
--
--     * @UPSERT@ : If a resource record set does not already exist, AWS creates it. If a resource set does exist, Route 53 updates it with the values in the request.
--
--
-- __Syntaxes for Creating, Updating, and Deleting Resource Record Sets__
-- The syntax for a request depends on the type of resource record set that you want to create, delete, or update, such as weighted, alias, or failover. The XML elements in your request must appear in the order listed in the syntax.
-- For an example for each type of resource record set, see "Examples."
-- Don't refer to the syntax in the "Parameter Syntax" section, which includes all of the elements for every kind of resource record set that you can create, delete, or update by using @ChangeResourceRecordSets@ .
-- __Change Propagation to Route 53 DNS Servers__
-- When you submit a @ChangeResourceRecordSets@ request, Route 53 propagates your changes to all of the Route 53 authoritative DNS servers. While your changes are propagating, @GetChange@ returns a status of @PENDING@ . When propagation is complete, @GetChange@ returns a status of @INSYNC@ . Changes generally propagate to all Route 53 name servers within 60 seconds. For more information, see <https://docs.aws.amazon.com/Route53/latest/APIReference/API_GetChange.html GetChange> .
-- __Limits on ChangeResourceRecordSets Requests__
-- For information about the limits on a @ChangeResourceRecordSets@ request, see <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/DNSLimitations.html Limits> in the /Amazon Route 53 Developer Guide/ .
module Network.AWS.Route53.ChangeResourceRecordSets
  ( -- * Creating a request
    ChangeResourceRecordSets (..),
    mkChangeResourceRecordSets,

    -- ** Request lenses
    crrsHostedZoneId,
    crrsChangeBatch,

    -- * Destructuring the response
    ChangeResourceRecordSetsResponse (..),
    mkChangeResourceRecordSetsResponse,

    -- ** Response lenses
    crrsrsResponseStatus,
    crrsrsChangeInfo,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Route53.Types

-- | A complex type that contains change information for the resource record set.
--
-- /See:/ 'mkChangeResourceRecordSets' smart constructor.
data ChangeResourceRecordSets = ChangeResourceRecordSets'
  { hostedZoneId ::
      ResourceId,
    changeBatch :: ChangeBatch
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ChangeResourceRecordSets' with the minimum fields required to make a request.
--
-- * 'changeBatch' - A complex type that contains an optional comment and the @Changes@ element.
-- * 'hostedZoneId' - The ID of the hosted zone that contains the resource record sets that you want to change.
mkChangeResourceRecordSets ::
  -- | 'hostedZoneId'
  ResourceId ->
  -- | 'changeBatch'
  ChangeBatch ->
  ChangeResourceRecordSets
mkChangeResourceRecordSets pHostedZoneId_ pChangeBatch_ =
  ChangeResourceRecordSets'
    { hostedZoneId = pHostedZoneId_,
      changeBatch = pChangeBatch_
    }

-- | The ID of the hosted zone that contains the resource record sets that you want to change.
--
-- /Note:/ Consider using 'hostedZoneId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crrsHostedZoneId :: Lens.Lens' ChangeResourceRecordSets ResourceId
crrsHostedZoneId = Lens.lens (hostedZoneId :: ChangeResourceRecordSets -> ResourceId) (\s a -> s {hostedZoneId = a} :: ChangeResourceRecordSets)
{-# DEPRECATED crrsHostedZoneId "Use generic-lens or generic-optics with 'hostedZoneId' instead." #-}

-- | A complex type that contains an optional comment and the @Changes@ element.
--
-- /Note:/ Consider using 'changeBatch' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crrsChangeBatch :: Lens.Lens' ChangeResourceRecordSets ChangeBatch
crrsChangeBatch = Lens.lens (changeBatch :: ChangeResourceRecordSets -> ChangeBatch) (\s a -> s {changeBatch = a} :: ChangeResourceRecordSets)
{-# DEPRECATED crrsChangeBatch "Use generic-lens or generic-optics with 'changeBatch' instead." #-}

instance Lude.AWSRequest ChangeResourceRecordSets where
  type Rs ChangeResourceRecordSets = ChangeResourceRecordSetsResponse
  request = Req.postXML route53Service
  response =
    Res.receiveXML
      ( \s h x ->
          ChangeResourceRecordSetsResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s)) Lude.<*> (x Lude..@ "ChangeInfo")
      )

instance Lude.ToElement ChangeResourceRecordSets where
  toElement =
    Lude.mkElement
      "{https://route53.amazonaws.com/doc/2013-04-01/}ChangeResourceRecordSetsRequest"

instance Lude.ToHeaders ChangeResourceRecordSets where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ChangeResourceRecordSets where
  toPath ChangeResourceRecordSets' {..} =
    Lude.mconcat
      ["/2013-04-01/hostedzone/", Lude.toBS hostedZoneId, "/rrset/"]

instance Lude.ToQuery ChangeResourceRecordSets where
  toQuery = Lude.const Lude.mempty

instance Lude.ToXML ChangeResourceRecordSets where
  toXML ChangeResourceRecordSets' {..} =
    Lude.mconcat ["ChangeBatch" Lude.@= changeBatch]

-- | A complex type containing the response for the request.
--
-- /See:/ 'mkChangeResourceRecordSetsResponse' smart constructor.
data ChangeResourceRecordSetsResponse = ChangeResourceRecordSetsResponse'
  { responseStatus ::
      Lude.Int,
    changeInfo :: ChangeInfo
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ChangeResourceRecordSetsResponse' with the minimum fields required to make a request.
--
-- * 'changeInfo' - A complex type that contains information about changes made to your hosted zone.
--
-- This element contains an ID that you use when performing a <https://docs.aws.amazon.com/Route53/latest/APIReference/API_GetChange.html GetChange> action to get detailed information about the change.
-- * 'responseStatus' - The response status code.
mkChangeResourceRecordSetsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'changeInfo'
  ChangeInfo ->
  ChangeResourceRecordSetsResponse
mkChangeResourceRecordSetsResponse pResponseStatus_ pChangeInfo_ =
  ChangeResourceRecordSetsResponse'
    { responseStatus =
        pResponseStatus_,
      changeInfo = pChangeInfo_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crrsrsResponseStatus :: Lens.Lens' ChangeResourceRecordSetsResponse Lude.Int
crrsrsResponseStatus = Lens.lens (responseStatus :: ChangeResourceRecordSetsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ChangeResourceRecordSetsResponse)
{-# DEPRECATED crrsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | A complex type that contains information about changes made to your hosted zone.
--
-- This element contains an ID that you use when performing a <https://docs.aws.amazon.com/Route53/latest/APIReference/API_GetChange.html GetChange> action to get detailed information about the change.
--
-- /Note:/ Consider using 'changeInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crrsrsChangeInfo :: Lens.Lens' ChangeResourceRecordSetsResponse ChangeInfo
crrsrsChangeInfo = Lens.lens (changeInfo :: ChangeResourceRecordSetsResponse -> ChangeInfo) (\s a -> s {changeInfo = a} :: ChangeResourceRecordSetsResponse)
{-# DEPRECATED crrsrsChangeInfo "Use generic-lens or generic-optics with 'changeInfo' instead." #-}
