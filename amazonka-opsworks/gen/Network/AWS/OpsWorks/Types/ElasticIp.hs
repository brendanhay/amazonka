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
-- Module      : Network.AWS.OpsWorks.Types.ElasticIp
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorks.Types.ElasticIp where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Describes an Elastic IP address.
--
-- /See:/ 'newElasticIp' smart constructor.
data ElasticIp = ElasticIp'
  { -- | The ID of the instance that the address is attached to.
    instanceId :: Core.Maybe Core.Text,
    -- | The IP address.
    ip :: Core.Maybe Core.Text,
    -- | The domain.
    domain :: Core.Maybe Core.Text,
    -- | The name.
    name :: Core.Maybe Core.Text,
    -- | The AWS region. For more information, see
    -- <https://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints>.
    region :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ElasticIp' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'elasticIp_instanceId' - The ID of the instance that the address is attached to.
--
-- 'ip', 'elasticIp_ip' - The IP address.
--
-- 'domain', 'elasticIp_domain' - The domain.
--
-- 'name', 'elasticIp_name' - The name.
--
-- 'region', 'elasticIp_region' - The AWS region. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints>.
newElasticIp ::
  ElasticIp
newElasticIp =
  ElasticIp'
    { instanceId = Core.Nothing,
      ip = Core.Nothing,
      domain = Core.Nothing,
      name = Core.Nothing,
      region = Core.Nothing
    }

-- | The ID of the instance that the address is attached to.
elasticIp_instanceId :: Lens.Lens' ElasticIp (Core.Maybe Core.Text)
elasticIp_instanceId = Lens.lens (\ElasticIp' {instanceId} -> instanceId) (\s@ElasticIp' {} a -> s {instanceId = a} :: ElasticIp)

-- | The IP address.
elasticIp_ip :: Lens.Lens' ElasticIp (Core.Maybe Core.Text)
elasticIp_ip = Lens.lens (\ElasticIp' {ip} -> ip) (\s@ElasticIp' {} a -> s {ip = a} :: ElasticIp)

-- | The domain.
elasticIp_domain :: Lens.Lens' ElasticIp (Core.Maybe Core.Text)
elasticIp_domain = Lens.lens (\ElasticIp' {domain} -> domain) (\s@ElasticIp' {} a -> s {domain = a} :: ElasticIp)

-- | The name.
elasticIp_name :: Lens.Lens' ElasticIp (Core.Maybe Core.Text)
elasticIp_name = Lens.lens (\ElasticIp' {name} -> name) (\s@ElasticIp' {} a -> s {name = a} :: ElasticIp)

-- | The AWS region. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints>.
elasticIp_region :: Lens.Lens' ElasticIp (Core.Maybe Core.Text)
elasticIp_region = Lens.lens (\ElasticIp' {region} -> region) (\s@ElasticIp' {} a -> s {region = a} :: ElasticIp)

instance Core.FromJSON ElasticIp where
  parseJSON =
    Core.withObject
      "ElasticIp"
      ( \x ->
          ElasticIp'
            Core.<$> (x Core..:? "InstanceId")
            Core.<*> (x Core..:? "Ip")
            Core.<*> (x Core..:? "Domain")
            Core.<*> (x Core..:? "Name")
            Core.<*> (x Core..:? "Region")
      )

instance Core.Hashable ElasticIp

instance Core.NFData ElasticIp
