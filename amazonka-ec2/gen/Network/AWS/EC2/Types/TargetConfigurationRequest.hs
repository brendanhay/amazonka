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
-- Module      : Network.AWS.EC2.Types.TargetConfigurationRequest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TargetConfigurationRequest where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens

-- | Details about the target configuration.
--
-- /See:/ 'newTargetConfigurationRequest' smart constructor.
data TargetConfigurationRequest = TargetConfigurationRequest'
  { -- | The number of instances the Covertible Reserved Instance offering can be
    -- applied to. This parameter is reserved and cannot be specified in a
    -- request
    instanceCount :: Core.Maybe Core.Int,
    -- | The Convertible Reserved Instance offering ID.
    offeringId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'TargetConfigurationRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceCount', 'targetConfigurationRequest_instanceCount' - The number of instances the Covertible Reserved Instance offering can be
-- applied to. This parameter is reserved and cannot be specified in a
-- request
--
-- 'offeringId', 'targetConfigurationRequest_offeringId' - The Convertible Reserved Instance offering ID.
newTargetConfigurationRequest ::
  -- | 'offeringId'
  Core.Text ->
  TargetConfigurationRequest
newTargetConfigurationRequest pOfferingId_ =
  TargetConfigurationRequest'
    { instanceCount =
        Core.Nothing,
      offeringId = pOfferingId_
    }

-- | The number of instances the Covertible Reserved Instance offering can be
-- applied to. This parameter is reserved and cannot be specified in a
-- request
targetConfigurationRequest_instanceCount :: Lens.Lens' TargetConfigurationRequest (Core.Maybe Core.Int)
targetConfigurationRequest_instanceCount = Lens.lens (\TargetConfigurationRequest' {instanceCount} -> instanceCount) (\s@TargetConfigurationRequest' {} a -> s {instanceCount = a} :: TargetConfigurationRequest)

-- | The Convertible Reserved Instance offering ID.
targetConfigurationRequest_offeringId :: Lens.Lens' TargetConfigurationRequest Core.Text
targetConfigurationRequest_offeringId = Lens.lens (\TargetConfigurationRequest' {offeringId} -> offeringId) (\s@TargetConfigurationRequest' {} a -> s {offeringId = a} :: TargetConfigurationRequest)

instance Core.Hashable TargetConfigurationRequest

instance Core.NFData TargetConfigurationRequest

instance Core.ToQuery TargetConfigurationRequest where
  toQuery TargetConfigurationRequest' {..} =
    Core.mconcat
      [ "InstanceCount" Core.=: instanceCount,
        "OfferingId" Core.=: offeringId
      ]
