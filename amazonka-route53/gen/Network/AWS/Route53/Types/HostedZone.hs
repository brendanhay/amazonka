{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.Types.HostedZone
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53.Types.HostedZone where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Route53.Internal
import Network.AWS.Route53.Types.HostedZoneConfig
import Network.AWS.Route53.Types.LinkedService

-- | A complex type that contains general information about the hosted zone.
--
-- /See:/ 'newHostedZone' smart constructor.
data HostedZone = HostedZone'
  { -- | The number of resource record sets in the hosted zone.
    resourceRecordSetCount :: Prelude.Maybe Prelude.Integer,
    -- | A complex type that includes the @Comment@ and @PrivateZone@ elements.
    -- If you omitted the @HostedZoneConfig@ and @Comment@ elements from the
    -- request, the @Config@ and @Comment@ elements don\'t appear in the
    -- response.
    config :: Prelude.Maybe HostedZoneConfig,
    -- | If the hosted zone was created by another service, the service that
    -- created the hosted zone. When a hosted zone is created by another
    -- service, you can\'t edit or delete it using Route 53.
    linkedService :: Prelude.Maybe LinkedService,
    -- | The ID that Amazon Route 53 assigned to the hosted zone when you created
    -- it.
    id :: ResourceId,
    -- | The name of the domain. For public hosted zones, this is the name that
    -- you have registered with your DNS registrar.
    --
    -- For information about how to specify characters other than @a-z@, @0-9@,
    -- and @-@ (hyphen) and how to specify internationalized domain names, see
    -- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_CreateHostedZone.html CreateHostedZone>.
    name :: Prelude.Text,
    -- | The value that you specified for @CallerReference@ when you created the
    -- hosted zone.
    callerReference :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'HostedZone' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceRecordSetCount', 'hostedZone_resourceRecordSetCount' - The number of resource record sets in the hosted zone.
--
-- 'config', 'hostedZone_config' - A complex type that includes the @Comment@ and @PrivateZone@ elements.
-- If you omitted the @HostedZoneConfig@ and @Comment@ elements from the
-- request, the @Config@ and @Comment@ elements don\'t appear in the
-- response.
--
-- 'linkedService', 'hostedZone_linkedService' - If the hosted zone was created by another service, the service that
-- created the hosted zone. When a hosted zone is created by another
-- service, you can\'t edit or delete it using Route 53.
--
-- 'id', 'hostedZone_id' - The ID that Amazon Route 53 assigned to the hosted zone when you created
-- it.
--
-- 'name', 'hostedZone_name' - The name of the domain. For public hosted zones, this is the name that
-- you have registered with your DNS registrar.
--
-- For information about how to specify characters other than @a-z@, @0-9@,
-- and @-@ (hyphen) and how to specify internationalized domain names, see
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_CreateHostedZone.html CreateHostedZone>.
--
-- 'callerReference', 'hostedZone_callerReference' - The value that you specified for @CallerReference@ when you created the
-- hosted zone.
newHostedZone ::
  -- | 'id'
  ResourceId ->
  -- | 'name'
  Prelude.Text ->
  -- | 'callerReference'
  Prelude.Text ->
  HostedZone
newHostedZone pId_ pName_ pCallerReference_ =
  HostedZone'
    { resourceRecordSetCount =
        Prelude.Nothing,
      config = Prelude.Nothing,
      linkedService = Prelude.Nothing,
      id = pId_,
      name = pName_,
      callerReference = pCallerReference_
    }

-- | The number of resource record sets in the hosted zone.
hostedZone_resourceRecordSetCount :: Lens.Lens' HostedZone (Prelude.Maybe Prelude.Integer)
hostedZone_resourceRecordSetCount = Lens.lens (\HostedZone' {resourceRecordSetCount} -> resourceRecordSetCount) (\s@HostedZone' {} a -> s {resourceRecordSetCount = a} :: HostedZone)

-- | A complex type that includes the @Comment@ and @PrivateZone@ elements.
-- If you omitted the @HostedZoneConfig@ and @Comment@ elements from the
-- request, the @Config@ and @Comment@ elements don\'t appear in the
-- response.
hostedZone_config :: Lens.Lens' HostedZone (Prelude.Maybe HostedZoneConfig)
hostedZone_config = Lens.lens (\HostedZone' {config} -> config) (\s@HostedZone' {} a -> s {config = a} :: HostedZone)

-- | If the hosted zone was created by another service, the service that
-- created the hosted zone. When a hosted zone is created by another
-- service, you can\'t edit or delete it using Route 53.
hostedZone_linkedService :: Lens.Lens' HostedZone (Prelude.Maybe LinkedService)
hostedZone_linkedService = Lens.lens (\HostedZone' {linkedService} -> linkedService) (\s@HostedZone' {} a -> s {linkedService = a} :: HostedZone)

-- | The ID that Amazon Route 53 assigned to the hosted zone when you created
-- it.
hostedZone_id :: Lens.Lens' HostedZone ResourceId
hostedZone_id = Lens.lens (\HostedZone' {id} -> id) (\s@HostedZone' {} a -> s {id = a} :: HostedZone)

-- | The name of the domain. For public hosted zones, this is the name that
-- you have registered with your DNS registrar.
--
-- For information about how to specify characters other than @a-z@, @0-9@,
-- and @-@ (hyphen) and how to specify internationalized domain names, see
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_CreateHostedZone.html CreateHostedZone>.
hostedZone_name :: Lens.Lens' HostedZone Prelude.Text
hostedZone_name = Lens.lens (\HostedZone' {name} -> name) (\s@HostedZone' {} a -> s {name = a} :: HostedZone)

-- | The value that you specified for @CallerReference@ when you created the
-- hosted zone.
hostedZone_callerReference :: Lens.Lens' HostedZone Prelude.Text
hostedZone_callerReference = Lens.lens (\HostedZone' {callerReference} -> callerReference) (\s@HostedZone' {} a -> s {callerReference = a} :: HostedZone)

instance Prelude.FromXML HostedZone where
  parseXML x =
    HostedZone'
      Prelude.<$> (x Prelude..@? "ResourceRecordSetCount")
      Prelude.<*> (x Prelude..@? "Config")
      Prelude.<*> (x Prelude..@? "LinkedService")
      Prelude.<*> (x Prelude..@ "Id")
      Prelude.<*> (x Prelude..@ "Name")
      Prelude.<*> (x Prelude..@ "CallerReference")

instance Prelude.Hashable HostedZone

instance Prelude.NFData HostedZone
