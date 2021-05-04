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
-- Module      : Network.AWS.OpsWorks.Types.ElasticIp
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorks.Types.ElasticIp where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes an Elastic IP address.
--
-- /See:/ 'newElasticIp' smart constructor.
data ElasticIp = ElasticIp'
  { -- | The ID of the instance that the address is attached to.
    instanceId :: Prelude.Maybe Prelude.Text,
    -- | The IP address.
    ip :: Prelude.Maybe Prelude.Text,
    -- | The domain.
    domain :: Prelude.Maybe Prelude.Text,
    -- | The name.
    name :: Prelude.Maybe Prelude.Text,
    -- | The AWS region. For more information, see
    -- <https://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints>.
    region :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { instanceId = Prelude.Nothing,
      ip = Prelude.Nothing,
      domain = Prelude.Nothing,
      name = Prelude.Nothing,
      region = Prelude.Nothing
    }

-- | The ID of the instance that the address is attached to.
elasticIp_instanceId :: Lens.Lens' ElasticIp (Prelude.Maybe Prelude.Text)
elasticIp_instanceId = Lens.lens (\ElasticIp' {instanceId} -> instanceId) (\s@ElasticIp' {} a -> s {instanceId = a} :: ElasticIp)

-- | The IP address.
elasticIp_ip :: Lens.Lens' ElasticIp (Prelude.Maybe Prelude.Text)
elasticIp_ip = Lens.lens (\ElasticIp' {ip} -> ip) (\s@ElasticIp' {} a -> s {ip = a} :: ElasticIp)

-- | The domain.
elasticIp_domain :: Lens.Lens' ElasticIp (Prelude.Maybe Prelude.Text)
elasticIp_domain = Lens.lens (\ElasticIp' {domain} -> domain) (\s@ElasticIp' {} a -> s {domain = a} :: ElasticIp)

-- | The name.
elasticIp_name :: Lens.Lens' ElasticIp (Prelude.Maybe Prelude.Text)
elasticIp_name = Lens.lens (\ElasticIp' {name} -> name) (\s@ElasticIp' {} a -> s {name = a} :: ElasticIp)

-- | The AWS region. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints>.
elasticIp_region :: Lens.Lens' ElasticIp (Prelude.Maybe Prelude.Text)
elasticIp_region = Lens.lens (\ElasticIp' {region} -> region) (\s@ElasticIp' {} a -> s {region = a} :: ElasticIp)

instance Prelude.FromJSON ElasticIp where
  parseJSON =
    Prelude.withObject
      "ElasticIp"
      ( \x ->
          ElasticIp'
            Prelude.<$> (x Prelude..:? "InstanceId")
            Prelude.<*> (x Prelude..:? "Ip")
            Prelude.<*> (x Prelude..:? "Domain")
            Prelude.<*> (x Prelude..:? "Name")
            Prelude.<*> (x Prelude..:? "Region")
      )

instance Prelude.Hashable ElasticIp

instance Prelude.NFData ElasticIp
