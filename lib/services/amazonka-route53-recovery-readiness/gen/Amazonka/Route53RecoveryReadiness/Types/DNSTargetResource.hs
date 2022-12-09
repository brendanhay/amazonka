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
-- Module      : Amazonka.Route53RecoveryReadiness.Types.DNSTargetResource
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53RecoveryReadiness.Types.DNSTargetResource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Route53RecoveryReadiness.Types.TargetResource

-- | A component for DNS\/routing control readiness checks and architecture
-- checks.
--
-- /See:/ 'newDNSTargetResource' smart constructor.
data DNSTargetResource = DNSTargetResource'
  { -- | The domain name that acts as an ingress point to a portion of the
    -- customer application.
    domainName :: Prelude.Maybe Prelude.Text,
    -- | The hosted zone Amazon Resource Name (ARN) that contains the DNS record
    -- with the provided name of the target resource.
    hostedZoneArn :: Prelude.Maybe Prelude.Text,
    -- | The Route 53 record set ID that uniquely identifies a DNS record, given
    -- a name and a type.
    recordSetId :: Prelude.Maybe Prelude.Text,
    -- | The type of DNS record of the target resource.
    recordType :: Prelude.Maybe Prelude.Text,
    -- | The target resource of the DNS target resource.
    targetResource :: Prelude.Maybe TargetResource
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DNSTargetResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainName', 'dNSTargetResource_domainName' - The domain name that acts as an ingress point to a portion of the
-- customer application.
--
-- 'hostedZoneArn', 'dNSTargetResource_hostedZoneArn' - The hosted zone Amazon Resource Name (ARN) that contains the DNS record
-- with the provided name of the target resource.
--
-- 'recordSetId', 'dNSTargetResource_recordSetId' - The Route 53 record set ID that uniquely identifies a DNS record, given
-- a name and a type.
--
-- 'recordType', 'dNSTargetResource_recordType' - The type of DNS record of the target resource.
--
-- 'targetResource', 'dNSTargetResource_targetResource' - The target resource of the DNS target resource.
newDNSTargetResource ::
  DNSTargetResource
newDNSTargetResource =
  DNSTargetResource'
    { domainName = Prelude.Nothing,
      hostedZoneArn = Prelude.Nothing,
      recordSetId = Prelude.Nothing,
      recordType = Prelude.Nothing,
      targetResource = Prelude.Nothing
    }

-- | The domain name that acts as an ingress point to a portion of the
-- customer application.
dNSTargetResource_domainName :: Lens.Lens' DNSTargetResource (Prelude.Maybe Prelude.Text)
dNSTargetResource_domainName = Lens.lens (\DNSTargetResource' {domainName} -> domainName) (\s@DNSTargetResource' {} a -> s {domainName = a} :: DNSTargetResource)

-- | The hosted zone Amazon Resource Name (ARN) that contains the DNS record
-- with the provided name of the target resource.
dNSTargetResource_hostedZoneArn :: Lens.Lens' DNSTargetResource (Prelude.Maybe Prelude.Text)
dNSTargetResource_hostedZoneArn = Lens.lens (\DNSTargetResource' {hostedZoneArn} -> hostedZoneArn) (\s@DNSTargetResource' {} a -> s {hostedZoneArn = a} :: DNSTargetResource)

-- | The Route 53 record set ID that uniquely identifies a DNS record, given
-- a name and a type.
dNSTargetResource_recordSetId :: Lens.Lens' DNSTargetResource (Prelude.Maybe Prelude.Text)
dNSTargetResource_recordSetId = Lens.lens (\DNSTargetResource' {recordSetId} -> recordSetId) (\s@DNSTargetResource' {} a -> s {recordSetId = a} :: DNSTargetResource)

-- | The type of DNS record of the target resource.
dNSTargetResource_recordType :: Lens.Lens' DNSTargetResource (Prelude.Maybe Prelude.Text)
dNSTargetResource_recordType = Lens.lens (\DNSTargetResource' {recordType} -> recordType) (\s@DNSTargetResource' {} a -> s {recordType = a} :: DNSTargetResource)

-- | The target resource of the DNS target resource.
dNSTargetResource_targetResource :: Lens.Lens' DNSTargetResource (Prelude.Maybe TargetResource)
dNSTargetResource_targetResource = Lens.lens (\DNSTargetResource' {targetResource} -> targetResource) (\s@DNSTargetResource' {} a -> s {targetResource = a} :: DNSTargetResource)

instance Data.FromJSON DNSTargetResource where
  parseJSON =
    Data.withObject
      "DNSTargetResource"
      ( \x ->
          DNSTargetResource'
            Prelude.<$> (x Data..:? "domainName")
            Prelude.<*> (x Data..:? "hostedZoneArn")
            Prelude.<*> (x Data..:? "recordSetId")
            Prelude.<*> (x Data..:? "recordType")
            Prelude.<*> (x Data..:? "targetResource")
      )

instance Prelude.Hashable DNSTargetResource where
  hashWithSalt _salt DNSTargetResource' {..} =
    _salt `Prelude.hashWithSalt` domainName
      `Prelude.hashWithSalt` hostedZoneArn
      `Prelude.hashWithSalt` recordSetId
      `Prelude.hashWithSalt` recordType
      `Prelude.hashWithSalt` targetResource

instance Prelude.NFData DNSTargetResource where
  rnf DNSTargetResource' {..} =
    Prelude.rnf domainName
      `Prelude.seq` Prelude.rnf hostedZoneArn
      `Prelude.seq` Prelude.rnf recordSetId
      `Prelude.seq` Prelude.rnf recordType
      `Prelude.seq` Prelude.rnf targetResource

instance Data.ToJSON DNSTargetResource where
  toJSON DNSTargetResource' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("domainName" Data..=) Prelude.<$> domainName,
            ("hostedZoneArn" Data..=) Prelude.<$> hostedZoneArn,
            ("recordSetId" Data..=) Prelude.<$> recordSetId,
            ("recordType" Data..=) Prelude.<$> recordType,
            ("targetResource" Data..=)
              Prelude.<$> targetResource
          ]
      )
