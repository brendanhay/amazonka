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
-- Module      : Network.AWS.XRay.Types.AnomalousService
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.AnomalousService where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.XRay.Types.ServiceId

-- | The service within the service graph that has anomalously high fault
-- rates.
--
-- /See:/ 'newAnomalousService' smart constructor.
data AnomalousService = AnomalousService'
  { serviceId :: Prelude.Maybe ServiceId
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AnomalousService' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serviceId', 'anomalousService_serviceId' - Undocumented member.
newAnomalousService ::
  AnomalousService
newAnomalousService =
  AnomalousService' {serviceId = Prelude.Nothing}

-- | Undocumented member.
anomalousService_serviceId :: Lens.Lens' AnomalousService (Prelude.Maybe ServiceId)
anomalousService_serviceId = Lens.lens (\AnomalousService' {serviceId} -> serviceId) (\s@AnomalousService' {} a -> s {serviceId = a} :: AnomalousService)

instance Prelude.FromJSON AnomalousService where
  parseJSON =
    Prelude.withObject
      "AnomalousService"
      ( \x ->
          AnomalousService'
            Prelude.<$> (x Prelude..:? "ServiceId")
      )

instance Prelude.Hashable AnomalousService

instance Prelude.NFData AnomalousService
