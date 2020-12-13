{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.Types.HostedZone
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53.Types.HostedZone
  ( HostedZone (..),

    -- * Smart constructor
    mkHostedZone,

    -- * Lenses
    hzLinkedService,
    hzConfig,
    hzName,
    hzId,
    hzResourceRecordSetCount,
    hzCallerReference,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Route53.Internal
import Network.AWS.Route53.Types.HostedZoneConfig
import Network.AWS.Route53.Types.LinkedService

-- | A complex type that contains general information about the hosted zone.
--
-- /See:/ 'mkHostedZone' smart constructor.
data HostedZone = HostedZone'
  { -- | If the hosted zone was created by another service, the service that created the hosted zone. When a hosted zone is created by another service, you can't edit or delete it using Route 53.
    linkedService :: Lude.Maybe LinkedService,
    -- | A complex type that includes the @Comment@ and @PrivateZone@ elements. If you omitted the @HostedZoneConfig@ and @Comment@ elements from the request, the @Config@ and @Comment@ elements don't appear in the response.
    config :: Lude.Maybe HostedZoneConfig,
    -- | The name of the domain. For public hosted zones, this is the name that you have registered with your DNS registrar.
    --
    -- For information about how to specify characters other than @a-z@ , @0-9@ , and @-@ (hyphen) and how to specify internationalized domain names, see <https://docs.aws.amazon.com/Route53/latest/APIReference/API_CreateHostedZone.html CreateHostedZone> .
    name :: Lude.Text,
    -- | The ID that Amazon Route 53 assigned to the hosted zone when you created it.
    id :: ResourceId,
    -- | The number of resource record sets in the hosted zone.
    resourceRecordSetCount :: Lude.Maybe Lude.Integer,
    -- | The value that you specified for @CallerReference@ when you created the hosted zone.
    callerReference :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'HostedZone' with the minimum fields required to make a request.
--
-- * 'linkedService' - If the hosted zone was created by another service, the service that created the hosted zone. When a hosted zone is created by another service, you can't edit or delete it using Route 53.
-- * 'config' - A complex type that includes the @Comment@ and @PrivateZone@ elements. If you omitted the @HostedZoneConfig@ and @Comment@ elements from the request, the @Config@ and @Comment@ elements don't appear in the response.
-- * 'name' - The name of the domain. For public hosted zones, this is the name that you have registered with your DNS registrar.
--
-- For information about how to specify characters other than @a-z@ , @0-9@ , and @-@ (hyphen) and how to specify internationalized domain names, see <https://docs.aws.amazon.com/Route53/latest/APIReference/API_CreateHostedZone.html CreateHostedZone> .
-- * 'id' - The ID that Amazon Route 53 assigned to the hosted zone when you created it.
-- * 'resourceRecordSetCount' - The number of resource record sets in the hosted zone.
-- * 'callerReference' - The value that you specified for @CallerReference@ when you created the hosted zone.
mkHostedZone ::
  -- | 'name'
  Lude.Text ->
  -- | 'id'
  ResourceId ->
  -- | 'callerReference'
  Lude.Text ->
  HostedZone
mkHostedZone pName_ pId_ pCallerReference_ =
  HostedZone'
    { linkedService = Lude.Nothing,
      config = Lude.Nothing,
      name = pName_,
      id = pId_,
      resourceRecordSetCount = Lude.Nothing,
      callerReference = pCallerReference_
    }

-- | If the hosted zone was created by another service, the service that created the hosted zone. When a hosted zone is created by another service, you can't edit or delete it using Route 53.
--
-- /Note:/ Consider using 'linkedService' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hzLinkedService :: Lens.Lens' HostedZone (Lude.Maybe LinkedService)
hzLinkedService = Lens.lens (linkedService :: HostedZone -> Lude.Maybe LinkedService) (\s a -> s {linkedService = a} :: HostedZone)
{-# DEPRECATED hzLinkedService "Use generic-lens or generic-optics with 'linkedService' instead." #-}

-- | A complex type that includes the @Comment@ and @PrivateZone@ elements. If you omitted the @HostedZoneConfig@ and @Comment@ elements from the request, the @Config@ and @Comment@ elements don't appear in the response.
--
-- /Note:/ Consider using 'config' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hzConfig :: Lens.Lens' HostedZone (Lude.Maybe HostedZoneConfig)
hzConfig = Lens.lens (config :: HostedZone -> Lude.Maybe HostedZoneConfig) (\s a -> s {config = a} :: HostedZone)
{-# DEPRECATED hzConfig "Use generic-lens or generic-optics with 'config' instead." #-}

-- | The name of the domain. For public hosted zones, this is the name that you have registered with your DNS registrar.
--
-- For information about how to specify characters other than @a-z@ , @0-9@ , and @-@ (hyphen) and how to specify internationalized domain names, see <https://docs.aws.amazon.com/Route53/latest/APIReference/API_CreateHostedZone.html CreateHostedZone> .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hzName :: Lens.Lens' HostedZone Lude.Text
hzName = Lens.lens (name :: HostedZone -> Lude.Text) (\s a -> s {name = a} :: HostedZone)
{-# DEPRECATED hzName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The ID that Amazon Route 53 assigned to the hosted zone when you created it.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hzId :: Lens.Lens' HostedZone ResourceId
hzId = Lens.lens (id :: HostedZone -> ResourceId) (\s a -> s {id = a} :: HostedZone)
{-# DEPRECATED hzId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The number of resource record sets in the hosted zone.
--
-- /Note:/ Consider using 'resourceRecordSetCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hzResourceRecordSetCount :: Lens.Lens' HostedZone (Lude.Maybe Lude.Integer)
hzResourceRecordSetCount = Lens.lens (resourceRecordSetCount :: HostedZone -> Lude.Maybe Lude.Integer) (\s a -> s {resourceRecordSetCount = a} :: HostedZone)
{-# DEPRECATED hzResourceRecordSetCount "Use generic-lens or generic-optics with 'resourceRecordSetCount' instead." #-}

-- | The value that you specified for @CallerReference@ when you created the hosted zone.
--
-- /Note:/ Consider using 'callerReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hzCallerReference :: Lens.Lens' HostedZone Lude.Text
hzCallerReference = Lens.lens (callerReference :: HostedZone -> Lude.Text) (\s a -> s {callerReference = a} :: HostedZone)
{-# DEPRECATED hzCallerReference "Use generic-lens or generic-optics with 'callerReference' instead." #-}

instance Lude.FromXML HostedZone where
  parseXML x =
    HostedZone'
      Lude.<$> (x Lude..@? "LinkedService")
      Lude.<*> (x Lude..@? "Config")
      Lude.<*> (x Lude..@ "Name")
      Lude.<*> (x Lude..@ "Id")
      Lude.<*> (x Lude..@? "ResourceRecordSetCount")
      Lude.<*> (x Lude..@ "CallerReference")
