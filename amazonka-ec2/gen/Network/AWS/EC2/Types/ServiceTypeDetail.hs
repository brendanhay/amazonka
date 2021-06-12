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
-- Module      : Network.AWS.EC2.Types.ServiceTypeDetail
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ServiceTypeDetail where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.ServiceType
import qualified Network.AWS.Lens as Lens

-- | Describes the type of service for a VPC endpoint.
--
-- /See:/ 'newServiceTypeDetail' smart constructor.
data ServiceTypeDetail = ServiceTypeDetail'
  { -- | The type of service.
    serviceType :: Core.Maybe ServiceType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  ServiceTypeDetail' {serviceType = Core.Nothing}

-- | The type of service.
serviceTypeDetail_serviceType :: Lens.Lens' ServiceTypeDetail (Core.Maybe ServiceType)
serviceTypeDetail_serviceType = Lens.lens (\ServiceTypeDetail' {serviceType} -> serviceType) (\s@ServiceTypeDetail' {} a -> s {serviceType = a} :: ServiceTypeDetail)

instance Core.FromXML ServiceTypeDetail where
  parseXML x =
    ServiceTypeDetail'
      Core.<$> (x Core..@? "serviceType")

instance Core.Hashable ServiceTypeDetail

instance Core.NFData ServiceTypeDetail
