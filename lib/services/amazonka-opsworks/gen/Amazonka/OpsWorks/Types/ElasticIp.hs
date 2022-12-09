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
-- Module      : Amazonka.OpsWorks.Types.ElasticIp
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpsWorks.Types.ElasticIp where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes an Elastic IP address.
--
-- /See:/ 'newElasticIp' smart constructor.
data ElasticIp = ElasticIp'
  { -- | The domain.
    domain :: Prelude.Maybe Prelude.Text,
    -- | The ID of the instance that the address is attached to.
    instanceId :: Prelude.Maybe Prelude.Text,
    -- | The IP address.
    ip :: Prelude.Maybe Prelude.Text,
    -- | The name.
    name :: Prelude.Maybe Prelude.Text,
    -- | The AWS region. For more information, see
    -- <https://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints>.
    region :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ElasticIp' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domain', 'elasticIp_domain' - The domain.
--
-- 'instanceId', 'elasticIp_instanceId' - The ID of the instance that the address is attached to.
--
-- 'ip', 'elasticIp_ip' - The IP address.
--
-- 'name', 'elasticIp_name' - The name.
--
-- 'region', 'elasticIp_region' - The AWS region. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints>.
newElasticIp ::
  ElasticIp
newElasticIp =
  ElasticIp'
    { domain = Prelude.Nothing,
      instanceId = Prelude.Nothing,
      ip = Prelude.Nothing,
      name = Prelude.Nothing,
      region = Prelude.Nothing
    }

-- | The domain.
elasticIp_domain :: Lens.Lens' ElasticIp (Prelude.Maybe Prelude.Text)
elasticIp_domain = Lens.lens (\ElasticIp' {domain} -> domain) (\s@ElasticIp' {} a -> s {domain = a} :: ElasticIp)

-- | The ID of the instance that the address is attached to.
elasticIp_instanceId :: Lens.Lens' ElasticIp (Prelude.Maybe Prelude.Text)
elasticIp_instanceId = Lens.lens (\ElasticIp' {instanceId} -> instanceId) (\s@ElasticIp' {} a -> s {instanceId = a} :: ElasticIp)

-- | The IP address.
elasticIp_ip :: Lens.Lens' ElasticIp (Prelude.Maybe Prelude.Text)
elasticIp_ip = Lens.lens (\ElasticIp' {ip} -> ip) (\s@ElasticIp' {} a -> s {ip = a} :: ElasticIp)

-- | The name.
elasticIp_name :: Lens.Lens' ElasticIp (Prelude.Maybe Prelude.Text)
elasticIp_name = Lens.lens (\ElasticIp' {name} -> name) (\s@ElasticIp' {} a -> s {name = a} :: ElasticIp)

-- | The AWS region. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints>.
elasticIp_region :: Lens.Lens' ElasticIp (Prelude.Maybe Prelude.Text)
elasticIp_region = Lens.lens (\ElasticIp' {region} -> region) (\s@ElasticIp' {} a -> s {region = a} :: ElasticIp)

instance Data.FromJSON ElasticIp where
  parseJSON =
    Data.withObject
      "ElasticIp"
      ( \x ->
          ElasticIp'
            Prelude.<$> (x Data..:? "Domain")
            Prelude.<*> (x Data..:? "InstanceId")
            Prelude.<*> (x Data..:? "Ip")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "Region")
      )

instance Prelude.Hashable ElasticIp where
  hashWithSalt _salt ElasticIp' {..} =
    _salt `Prelude.hashWithSalt` domain
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` ip
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` region

instance Prelude.NFData ElasticIp where
  rnf ElasticIp' {..} =
    Prelude.rnf domain
      `Prelude.seq` Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf ip
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf region
