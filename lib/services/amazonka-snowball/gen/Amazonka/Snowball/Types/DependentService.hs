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
-- Module      : Amazonka.Snowball.Types.DependentService
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Snowball.Types.DependentService where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Snowball.Types.ServiceName
import Amazonka.Snowball.Types.ServiceVersion

-- | The name and version of the service dependant on the requested service.
--
-- /See:/ 'newDependentService' smart constructor.
data DependentService = DependentService'
  { -- | The name of the dependent service.
    serviceName :: Prelude.Maybe ServiceName,
    -- | The version of the dependent service.
    serviceVersion :: Prelude.Maybe ServiceVersion
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DependentService' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serviceName', 'dependentService_serviceName' - The name of the dependent service.
--
-- 'serviceVersion', 'dependentService_serviceVersion' - The version of the dependent service.
newDependentService ::
  DependentService
newDependentService =
  DependentService'
    { serviceName = Prelude.Nothing,
      serviceVersion = Prelude.Nothing
    }

-- | The name of the dependent service.
dependentService_serviceName :: Lens.Lens' DependentService (Prelude.Maybe ServiceName)
dependentService_serviceName = Lens.lens (\DependentService' {serviceName} -> serviceName) (\s@DependentService' {} a -> s {serviceName = a} :: DependentService)

-- | The version of the dependent service.
dependentService_serviceVersion :: Lens.Lens' DependentService (Prelude.Maybe ServiceVersion)
dependentService_serviceVersion = Lens.lens (\DependentService' {serviceVersion} -> serviceVersion) (\s@DependentService' {} a -> s {serviceVersion = a} :: DependentService)

instance Data.FromJSON DependentService where
  parseJSON =
    Data.withObject
      "DependentService"
      ( \x ->
          DependentService'
            Prelude.<$> (x Data..:? "ServiceName")
            Prelude.<*> (x Data..:? "ServiceVersion")
      )

instance Prelude.Hashable DependentService where
  hashWithSalt _salt DependentService' {..} =
    _salt
      `Prelude.hashWithSalt` serviceName
      `Prelude.hashWithSalt` serviceVersion

instance Prelude.NFData DependentService where
  rnf DependentService' {..} =
    Prelude.rnf serviceName
      `Prelude.seq` Prelude.rnf serviceVersion

instance Data.ToJSON DependentService where
  toJSON DependentService' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ServiceName" Data..=) Prelude.<$> serviceName,
            ("ServiceVersion" Data..=)
              Prelude.<$> serviceVersion
          ]
      )
