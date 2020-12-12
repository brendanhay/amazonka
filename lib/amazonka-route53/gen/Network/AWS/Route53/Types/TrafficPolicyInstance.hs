{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.Types.TrafficPolicyInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53.Types.TrafficPolicyInstance
  ( TrafficPolicyInstance (..),

    -- * Smart constructor
    mkTrafficPolicyInstance,

    -- * Lenses
    tpiId,
    tpiHostedZoneId,
    tpiName,
    tpiTTL,
    tpiState,
    tpiMessage,
    tpiTrafficPolicyId,
    tpiTrafficPolicyVersion,
    tpiTrafficPolicyType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Route53.Internal
import Network.AWS.Route53.Types.RecordType

-- | A complex type that contains settings for the new traffic policy instance.
--
-- /See:/ 'mkTrafficPolicyInstance' smart constructor.
data TrafficPolicyInstance = TrafficPolicyInstance'
  { id ::
      Lude.Text,
    hostedZoneId :: ResourceId,
    name :: Lude.Text,
    tTL :: Lude.Natural,
    state :: Lude.Text,
    message :: Lude.Text,
    trafficPolicyId :: Lude.Text,
    trafficPolicyVersion :: Lude.Natural,
    trafficPolicyType :: RecordType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TrafficPolicyInstance' with the minimum fields required to make a request.
--
-- * 'hostedZoneId' - The ID of the hosted zone that Amazon Route 53 created resource record sets in.
-- * 'id' - The ID that Amazon Route 53 assigned to the new traffic policy instance.
-- * 'message' - If @State@ is @Failed@ , an explanation of the reason for the failure. If @State@ is another value, @Message@ is empty.
-- * 'name' - The DNS name, such as www.example.com, for which Amazon Route 53 responds to queries by using the resource record sets that are associated with this traffic policy instance.
-- * 'state' - The value of @State@ is one of the following values:
--
--
--     * Applied
--
--     * Amazon Route 53 has finished creating resource record sets, and changes have propagated to all Route 53 edge locations.
--
--
--     * Creating
--
--     * Route 53 is creating the resource record sets. Use @GetTrafficPolicyInstance@ to confirm that the @CreateTrafficPolicyInstance@ request completed successfully.
--
--
--     * Failed
--
--     * Route 53 wasn't able to create or update the resource record sets. When the value of @State@ is @Failed@ , see @Message@ for an explanation of what caused the request to fail.
--
--
-- * 'tTL' - The TTL that Amazon Route 53 assigned to all of the resource record sets that it created in the specified hosted zone.
-- * 'trafficPolicyId' - The ID of the traffic policy that Amazon Route 53 used to create resource record sets in the specified hosted zone.
-- * 'trafficPolicyType' - The DNS type that Amazon Route 53 assigned to all of the resource record sets that it created for this traffic policy instance.
-- * 'trafficPolicyVersion' - The version of the traffic policy that Amazon Route 53 used to create resource record sets in the specified hosted zone.
mkTrafficPolicyInstance ::
  -- | 'id'
  Lude.Text ->
  -- | 'hostedZoneId'
  ResourceId ->
  -- | 'name'
  Lude.Text ->
  -- | 'tTL'
  Lude.Natural ->
  -- | 'state'
  Lude.Text ->
  -- | 'message'
  Lude.Text ->
  -- | 'trafficPolicyId'
  Lude.Text ->
  -- | 'trafficPolicyVersion'
  Lude.Natural ->
  -- | 'trafficPolicyType'
  RecordType ->
  TrafficPolicyInstance
mkTrafficPolicyInstance
  pId_
  pHostedZoneId_
  pName_
  pTTL_
  pState_
  pMessage_
  pTrafficPolicyId_
  pTrafficPolicyVersion_
  pTrafficPolicyType_ =
    TrafficPolicyInstance'
      { id = pId_,
        hostedZoneId = pHostedZoneId_,
        name = pName_,
        tTL = pTTL_,
        state = pState_,
        message = pMessage_,
        trafficPolicyId = pTrafficPolicyId_,
        trafficPolicyVersion = pTrafficPolicyVersion_,
        trafficPolicyType = pTrafficPolicyType_
      }

-- | The ID that Amazon Route 53 assigned to the new traffic policy instance.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tpiId :: Lens.Lens' TrafficPolicyInstance Lude.Text
tpiId = Lens.lens (id :: TrafficPolicyInstance -> Lude.Text) (\s a -> s {id = a} :: TrafficPolicyInstance)
{-# DEPRECATED tpiId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The ID of the hosted zone that Amazon Route 53 created resource record sets in.
--
-- /Note:/ Consider using 'hostedZoneId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tpiHostedZoneId :: Lens.Lens' TrafficPolicyInstance ResourceId
tpiHostedZoneId = Lens.lens (hostedZoneId :: TrafficPolicyInstance -> ResourceId) (\s a -> s {hostedZoneId = a} :: TrafficPolicyInstance)
{-# DEPRECATED tpiHostedZoneId "Use generic-lens or generic-optics with 'hostedZoneId' instead." #-}

-- | The DNS name, such as www.example.com, for which Amazon Route 53 responds to queries by using the resource record sets that are associated with this traffic policy instance.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tpiName :: Lens.Lens' TrafficPolicyInstance Lude.Text
tpiName = Lens.lens (name :: TrafficPolicyInstance -> Lude.Text) (\s a -> s {name = a} :: TrafficPolicyInstance)
{-# DEPRECATED tpiName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The TTL that Amazon Route 53 assigned to all of the resource record sets that it created in the specified hosted zone.
--
-- /Note:/ Consider using 'tTL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tpiTTL :: Lens.Lens' TrafficPolicyInstance Lude.Natural
tpiTTL = Lens.lens (tTL :: TrafficPolicyInstance -> Lude.Natural) (\s a -> s {tTL = a} :: TrafficPolicyInstance)
{-# DEPRECATED tpiTTL "Use generic-lens or generic-optics with 'tTL' instead." #-}

-- | The value of @State@ is one of the following values:
--
--
--     * Applied
--
--     * Amazon Route 53 has finished creating resource record sets, and changes have propagated to all Route 53 edge locations.
--
--
--     * Creating
--
--     * Route 53 is creating the resource record sets. Use @GetTrafficPolicyInstance@ to confirm that the @CreateTrafficPolicyInstance@ request completed successfully.
--
--
--     * Failed
--
--     * Route 53 wasn't able to create or update the resource record sets. When the value of @State@ is @Failed@ , see @Message@ for an explanation of what caused the request to fail.
--
--
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tpiState :: Lens.Lens' TrafficPolicyInstance Lude.Text
tpiState = Lens.lens (state :: TrafficPolicyInstance -> Lude.Text) (\s a -> s {state = a} :: TrafficPolicyInstance)
{-# DEPRECATED tpiState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | If @State@ is @Failed@ , an explanation of the reason for the failure. If @State@ is another value, @Message@ is empty.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tpiMessage :: Lens.Lens' TrafficPolicyInstance Lude.Text
tpiMessage = Lens.lens (message :: TrafficPolicyInstance -> Lude.Text) (\s a -> s {message = a} :: TrafficPolicyInstance)
{-# DEPRECATED tpiMessage "Use generic-lens or generic-optics with 'message' instead." #-}

-- | The ID of the traffic policy that Amazon Route 53 used to create resource record sets in the specified hosted zone.
--
-- /Note:/ Consider using 'trafficPolicyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tpiTrafficPolicyId :: Lens.Lens' TrafficPolicyInstance Lude.Text
tpiTrafficPolicyId = Lens.lens (trafficPolicyId :: TrafficPolicyInstance -> Lude.Text) (\s a -> s {trafficPolicyId = a} :: TrafficPolicyInstance)
{-# DEPRECATED tpiTrafficPolicyId "Use generic-lens or generic-optics with 'trafficPolicyId' instead." #-}

-- | The version of the traffic policy that Amazon Route 53 used to create resource record sets in the specified hosted zone.
--
-- /Note:/ Consider using 'trafficPolicyVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tpiTrafficPolicyVersion :: Lens.Lens' TrafficPolicyInstance Lude.Natural
tpiTrafficPolicyVersion = Lens.lens (trafficPolicyVersion :: TrafficPolicyInstance -> Lude.Natural) (\s a -> s {trafficPolicyVersion = a} :: TrafficPolicyInstance)
{-# DEPRECATED tpiTrafficPolicyVersion "Use generic-lens or generic-optics with 'trafficPolicyVersion' instead." #-}

-- | The DNS type that Amazon Route 53 assigned to all of the resource record sets that it created for this traffic policy instance.
--
-- /Note:/ Consider using 'trafficPolicyType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tpiTrafficPolicyType :: Lens.Lens' TrafficPolicyInstance RecordType
tpiTrafficPolicyType = Lens.lens (trafficPolicyType :: TrafficPolicyInstance -> RecordType) (\s a -> s {trafficPolicyType = a} :: TrafficPolicyInstance)
{-# DEPRECATED tpiTrafficPolicyType "Use generic-lens or generic-optics with 'trafficPolicyType' instead." #-}

instance Lude.FromXML TrafficPolicyInstance where
  parseXML x =
    TrafficPolicyInstance'
      Lude.<$> (x Lude..@ "Id")
      Lude.<*> (x Lude..@ "HostedZoneId")
      Lude.<*> (x Lude..@ "Name")
      Lude.<*> (x Lude..@ "TTL")
      Lude.<*> (x Lude..@ "State")
      Lude.<*> (x Lude..@ "Message")
      Lude.<*> (x Lude..@ "TrafficPolicyId")
      Lude.<*> (x Lude..@ "TrafficPolicyVersion")
      Lude.<*> (x Lude..@ "TrafficPolicyType")
