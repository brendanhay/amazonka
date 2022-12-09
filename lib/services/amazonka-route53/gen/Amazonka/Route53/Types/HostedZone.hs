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
-- Module      : Amazonka.Route53.Types.HostedZone
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53.Types.HostedZone where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Route53.Internal
import Amazonka.Route53.Types.HostedZoneConfig
import Amazonka.Route53.Types.LinkedService

-- | A complex type that contains general information about the hosted zone.
--
-- /See:/ 'newHostedZone' smart constructor.
data HostedZone = HostedZone'
  { -- | A complex type that includes the @Comment@ and @PrivateZone@ elements.
    -- If you omitted the @HostedZoneConfig@ and @Comment@ elements from the
    -- request, the @Config@ and @Comment@ elements don\'t appear in the
    -- response.
    config :: Prelude.Maybe HostedZoneConfig,
    -- | If the hosted zone was created by another service, the service that
    -- created the hosted zone. When a hosted zone is created by another
    -- service, you can\'t edit or delete it using Route 53.
    linkedService :: Prelude.Maybe LinkedService,
    -- | The number of resource record sets in the hosted zone.
    resourceRecordSetCount :: Prelude.Maybe Prelude.Integer,
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HostedZone' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
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
-- 'resourceRecordSetCount', 'hostedZone_resourceRecordSetCount' - The number of resource record sets in the hosted zone.
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
    { config = Prelude.Nothing,
      linkedService = Prelude.Nothing,
      resourceRecordSetCount = Prelude.Nothing,
      id = pId_,
      name = pName_,
      callerReference = pCallerReference_
    }

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

-- | The number of resource record sets in the hosted zone.
hostedZone_resourceRecordSetCount :: Lens.Lens' HostedZone (Prelude.Maybe Prelude.Integer)
hostedZone_resourceRecordSetCount = Lens.lens (\HostedZone' {resourceRecordSetCount} -> resourceRecordSetCount) (\s@HostedZone' {} a -> s {resourceRecordSetCount = a} :: HostedZone)

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

instance Data.FromXML HostedZone where
  parseXML x =
    HostedZone'
      Prelude.<$> (x Data..@? "Config")
      Prelude.<*> (x Data..@? "LinkedService")
      Prelude.<*> (x Data..@? "ResourceRecordSetCount")
      Prelude.<*> (x Data..@ "Id")
      Prelude.<*> (x Data..@ "Name")
      Prelude.<*> (x Data..@ "CallerReference")

instance Prelude.Hashable HostedZone where
  hashWithSalt _salt HostedZone' {..} =
    _salt `Prelude.hashWithSalt` config
      `Prelude.hashWithSalt` linkedService
      `Prelude.hashWithSalt` resourceRecordSetCount
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` callerReference

instance Prelude.NFData HostedZone where
  rnf HostedZone' {..} =
    Prelude.rnf config
      `Prelude.seq` Prelude.rnf linkedService
      `Prelude.seq` Prelude.rnf resourceRecordSetCount
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf callerReference
