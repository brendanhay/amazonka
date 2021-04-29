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
-- Module      : Network.AWS.Lightsail.Types.Origin
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.Origin where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types.OriginProtocolPolicyEnum
import Network.AWS.Lightsail.Types.RegionName
import Network.AWS.Lightsail.Types.ResourceType
import qualified Network.AWS.Prelude as Prelude

-- | Describes the origin resource of an Amazon Lightsail content delivery
-- network (CDN) distribution.
--
-- An origin can be a Lightsail instance or load balancer. A distribution
-- pulls content from an origin, caches it, and serves it to viewers via a
-- worldwide network of edge servers.
--
-- /See:/ 'newOrigin' smart constructor.
data Origin = Origin'
  { -- | The AWS Region name of the origin resource.
    regionName :: Prelude.Maybe RegionName,
    -- | The protocol that your Amazon Lightsail distribution uses when
    -- establishing a connection with your origin to pull content.
    protocolPolicy :: Prelude.Maybe OriginProtocolPolicyEnum,
    -- | The resource type of the origin resource (e.g., /Instance/).
    resourceType :: Prelude.Maybe ResourceType,
    -- | The name of the origin resource.
    name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Origin' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'regionName', 'origin_regionName' - The AWS Region name of the origin resource.
--
-- 'protocolPolicy', 'origin_protocolPolicy' - The protocol that your Amazon Lightsail distribution uses when
-- establishing a connection with your origin to pull content.
--
-- 'resourceType', 'origin_resourceType' - The resource type of the origin resource (e.g., /Instance/).
--
-- 'name', 'origin_name' - The name of the origin resource.
newOrigin ::
  Origin
newOrigin =
  Origin'
    { regionName = Prelude.Nothing,
      protocolPolicy = Prelude.Nothing,
      resourceType = Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | The AWS Region name of the origin resource.
origin_regionName :: Lens.Lens' Origin (Prelude.Maybe RegionName)
origin_regionName = Lens.lens (\Origin' {regionName} -> regionName) (\s@Origin' {} a -> s {regionName = a} :: Origin)

-- | The protocol that your Amazon Lightsail distribution uses when
-- establishing a connection with your origin to pull content.
origin_protocolPolicy :: Lens.Lens' Origin (Prelude.Maybe OriginProtocolPolicyEnum)
origin_protocolPolicy = Lens.lens (\Origin' {protocolPolicy} -> protocolPolicy) (\s@Origin' {} a -> s {protocolPolicy = a} :: Origin)

-- | The resource type of the origin resource (e.g., /Instance/).
origin_resourceType :: Lens.Lens' Origin (Prelude.Maybe ResourceType)
origin_resourceType = Lens.lens (\Origin' {resourceType} -> resourceType) (\s@Origin' {} a -> s {resourceType = a} :: Origin)

-- | The name of the origin resource.
origin_name :: Lens.Lens' Origin (Prelude.Maybe Prelude.Text)
origin_name = Lens.lens (\Origin' {name} -> name) (\s@Origin' {} a -> s {name = a} :: Origin)

instance Prelude.FromJSON Origin where
  parseJSON =
    Prelude.withObject
      "Origin"
      ( \x ->
          Origin'
            Prelude.<$> (x Prelude..:? "regionName")
            Prelude.<*> (x Prelude..:? "protocolPolicy")
            Prelude.<*> (x Prelude..:? "resourceType")
            Prelude.<*> (x Prelude..:? "name")
      )

instance Prelude.Hashable Origin

instance Prelude.NFData Origin
