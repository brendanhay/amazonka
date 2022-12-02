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
-- Module      : Amazonka.AppMesh.Types.ServiceDiscovery
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppMesh.Types.ServiceDiscovery where

import Amazonka.AppMesh.Types.AwsCloudMapServiceDiscovery
import Amazonka.AppMesh.Types.DnsServiceDiscovery
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object that represents the service discovery information for a
-- virtual node.
--
-- /See:/ 'newServiceDiscovery' smart constructor.
data ServiceDiscovery = ServiceDiscovery'
  { -- | Specifies the DNS information for the virtual node.
    dns :: Prelude.Maybe DnsServiceDiscovery,
    -- | Specifies any Cloud Map information for the virtual node.
    awsCloudMap :: Prelude.Maybe AwsCloudMapServiceDiscovery
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ServiceDiscovery' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dns', 'serviceDiscovery_dns' - Specifies the DNS information for the virtual node.
--
-- 'awsCloudMap', 'serviceDiscovery_awsCloudMap' - Specifies any Cloud Map information for the virtual node.
newServiceDiscovery ::
  ServiceDiscovery
newServiceDiscovery =
  ServiceDiscovery'
    { dns = Prelude.Nothing,
      awsCloudMap = Prelude.Nothing
    }

-- | Specifies the DNS information for the virtual node.
serviceDiscovery_dns :: Lens.Lens' ServiceDiscovery (Prelude.Maybe DnsServiceDiscovery)
serviceDiscovery_dns = Lens.lens (\ServiceDiscovery' {dns} -> dns) (\s@ServiceDiscovery' {} a -> s {dns = a} :: ServiceDiscovery)

-- | Specifies any Cloud Map information for the virtual node.
serviceDiscovery_awsCloudMap :: Lens.Lens' ServiceDiscovery (Prelude.Maybe AwsCloudMapServiceDiscovery)
serviceDiscovery_awsCloudMap = Lens.lens (\ServiceDiscovery' {awsCloudMap} -> awsCloudMap) (\s@ServiceDiscovery' {} a -> s {awsCloudMap = a} :: ServiceDiscovery)

instance Data.FromJSON ServiceDiscovery where
  parseJSON =
    Data.withObject
      "ServiceDiscovery"
      ( \x ->
          ServiceDiscovery'
            Prelude.<$> (x Data..:? "dns")
            Prelude.<*> (x Data..:? "awsCloudMap")
      )

instance Prelude.Hashable ServiceDiscovery where
  hashWithSalt _salt ServiceDiscovery' {..} =
    _salt `Prelude.hashWithSalt` dns
      `Prelude.hashWithSalt` awsCloudMap

instance Prelude.NFData ServiceDiscovery where
  rnf ServiceDiscovery' {..} =
    Prelude.rnf dns
      `Prelude.seq` Prelude.rnf awsCloudMap

instance Data.ToJSON ServiceDiscovery where
  toJSON ServiceDiscovery' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("dns" Data..=) Prelude.<$> dns,
            ("awsCloudMap" Data..=) Prelude.<$> awsCloudMap
          ]
      )
