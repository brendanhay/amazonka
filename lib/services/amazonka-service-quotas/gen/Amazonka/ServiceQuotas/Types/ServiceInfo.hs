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
-- Module      : Amazonka.ServiceQuotas.Types.ServiceInfo
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ServiceQuotas.Types.ServiceInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about a service.
--
-- /See:/ 'newServiceInfo' smart constructor.
data ServiceInfo = ServiceInfo'
  { -- | The service identifier.
    serviceCode :: Prelude.Maybe Prelude.Text,
    -- | The service name.
    serviceName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ServiceInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serviceCode', 'serviceInfo_serviceCode' - The service identifier.
--
-- 'serviceName', 'serviceInfo_serviceName' - The service name.
newServiceInfo ::
  ServiceInfo
newServiceInfo =
  ServiceInfo'
    { serviceCode = Prelude.Nothing,
      serviceName = Prelude.Nothing
    }

-- | The service identifier.
serviceInfo_serviceCode :: Lens.Lens' ServiceInfo (Prelude.Maybe Prelude.Text)
serviceInfo_serviceCode = Lens.lens (\ServiceInfo' {serviceCode} -> serviceCode) (\s@ServiceInfo' {} a -> s {serviceCode = a} :: ServiceInfo)

-- | The service name.
serviceInfo_serviceName :: Lens.Lens' ServiceInfo (Prelude.Maybe Prelude.Text)
serviceInfo_serviceName = Lens.lens (\ServiceInfo' {serviceName} -> serviceName) (\s@ServiceInfo' {} a -> s {serviceName = a} :: ServiceInfo)

instance Data.FromJSON ServiceInfo where
  parseJSON =
    Data.withObject
      "ServiceInfo"
      ( \x ->
          ServiceInfo'
            Prelude.<$> (x Data..:? "ServiceCode")
            Prelude.<*> (x Data..:? "ServiceName")
      )

instance Prelude.Hashable ServiceInfo where
  hashWithSalt _salt ServiceInfo' {..} =
    _salt
      `Prelude.hashWithSalt` serviceCode
      `Prelude.hashWithSalt` serviceName

instance Prelude.NFData ServiceInfo where
  rnf ServiceInfo' {..} =
    Prelude.rnf serviceCode
      `Prelude.seq` Prelude.rnf serviceName
