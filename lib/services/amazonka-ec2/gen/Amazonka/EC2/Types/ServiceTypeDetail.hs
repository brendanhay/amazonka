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
-- Module      : Amazonka.EC2.Types.ServiceTypeDetail
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.ServiceTypeDetail where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.ServiceType
import qualified Amazonka.Prelude as Prelude

-- | Describes the type of service for a VPC endpoint.
--
-- /See:/ 'newServiceTypeDetail' smart constructor.
data ServiceTypeDetail = ServiceTypeDetail'
  { -- | The type of service.
    serviceType :: Prelude.Maybe ServiceType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ServiceTypeDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serviceType', 'serviceTypeDetail_serviceType' - The type of service.
newServiceTypeDetail ::
  ServiceTypeDetail
newServiceTypeDetail =
  ServiceTypeDetail' {serviceType = Prelude.Nothing}

-- | The type of service.
serviceTypeDetail_serviceType :: Lens.Lens' ServiceTypeDetail (Prelude.Maybe ServiceType)
serviceTypeDetail_serviceType = Lens.lens (\ServiceTypeDetail' {serviceType} -> serviceType) (\s@ServiceTypeDetail' {} a -> s {serviceType = a} :: ServiceTypeDetail)

instance Data.FromXML ServiceTypeDetail where
  parseXML x =
    ServiceTypeDetail'
      Prelude.<$> (x Data..@? "serviceType")

instance Prelude.Hashable ServiceTypeDetail where
  hashWithSalt _salt ServiceTypeDetail' {..} =
    _salt `Prelude.hashWithSalt` serviceType

instance Prelude.NFData ServiceTypeDetail where
  rnf ServiceTypeDetail' {..} = Prelude.rnf serviceType
