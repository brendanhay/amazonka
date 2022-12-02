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
-- Module      : Amazonka.Lightsail.Types.Origin
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lightsail.Types.Origin where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lightsail.Types.OriginProtocolPolicyEnum
import Amazonka.Lightsail.Types.RegionName
import Amazonka.Lightsail.Types.ResourceType
import qualified Amazonka.Prelude as Prelude

-- | Describes the origin resource of an Amazon Lightsail content delivery
-- network (CDN) distribution.
--
-- An origin can be a Lightsail instance, bucket, or load balancer. A
-- distribution pulls content from an origin, caches it, and serves it to
-- viewers via a worldwide network of edge servers.
--
-- /See:/ 'newOrigin' smart constructor.
data Origin = Origin'
  { -- | The resource type of the origin resource (e.g., /Instance/).
    resourceType :: Prelude.Maybe ResourceType,
    -- | The name of the origin resource.
    name :: Prelude.Maybe Prelude.Text,
    -- | The protocol that your Amazon Lightsail distribution uses when
    -- establishing a connection with your origin to pull content.
    protocolPolicy :: Prelude.Maybe OriginProtocolPolicyEnum,
    -- | The AWS Region name of the origin resource.
    regionName :: Prelude.Maybe RegionName
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Origin' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceType', 'origin_resourceType' - The resource type of the origin resource (e.g., /Instance/).
--
-- 'name', 'origin_name' - The name of the origin resource.
--
-- 'protocolPolicy', 'origin_protocolPolicy' - The protocol that your Amazon Lightsail distribution uses when
-- establishing a connection with your origin to pull content.
--
-- 'regionName', 'origin_regionName' - The AWS Region name of the origin resource.
newOrigin ::
  Origin
newOrigin =
  Origin'
    { resourceType = Prelude.Nothing,
      name = Prelude.Nothing,
      protocolPolicy = Prelude.Nothing,
      regionName = Prelude.Nothing
    }

-- | The resource type of the origin resource (e.g., /Instance/).
origin_resourceType :: Lens.Lens' Origin (Prelude.Maybe ResourceType)
origin_resourceType = Lens.lens (\Origin' {resourceType} -> resourceType) (\s@Origin' {} a -> s {resourceType = a} :: Origin)

-- | The name of the origin resource.
origin_name :: Lens.Lens' Origin (Prelude.Maybe Prelude.Text)
origin_name = Lens.lens (\Origin' {name} -> name) (\s@Origin' {} a -> s {name = a} :: Origin)

-- | The protocol that your Amazon Lightsail distribution uses when
-- establishing a connection with your origin to pull content.
origin_protocolPolicy :: Lens.Lens' Origin (Prelude.Maybe OriginProtocolPolicyEnum)
origin_protocolPolicy = Lens.lens (\Origin' {protocolPolicy} -> protocolPolicy) (\s@Origin' {} a -> s {protocolPolicy = a} :: Origin)

-- | The AWS Region name of the origin resource.
origin_regionName :: Lens.Lens' Origin (Prelude.Maybe RegionName)
origin_regionName = Lens.lens (\Origin' {regionName} -> regionName) (\s@Origin' {} a -> s {regionName = a} :: Origin)

instance Data.FromJSON Origin where
  parseJSON =
    Data.withObject
      "Origin"
      ( \x ->
          Origin'
            Prelude.<$> (x Data..:? "resourceType")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "protocolPolicy")
            Prelude.<*> (x Data..:? "regionName")
      )

instance Prelude.Hashable Origin where
  hashWithSalt _salt Origin' {..} =
    _salt `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` protocolPolicy
      `Prelude.hashWithSalt` regionName

instance Prelude.NFData Origin where
  rnf Origin' {..} =
    Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf protocolPolicy
      `Prelude.seq` Prelude.rnf regionName
