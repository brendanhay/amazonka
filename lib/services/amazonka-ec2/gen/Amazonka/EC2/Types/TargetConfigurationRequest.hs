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
-- Module      : Amazonka.EC2.Types.TargetConfigurationRequest
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.TargetConfigurationRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | Details about the target configuration.
--
-- /See:/ 'newTargetConfigurationRequest' smart constructor.
data TargetConfigurationRequest = TargetConfigurationRequest'
  { -- | The number of instances the Convertible Reserved Instance offering can
    -- be applied to. This parameter is reserved and cannot be specified in a
    -- request
    instanceCount :: Prelude.Maybe Prelude.Int,
    -- | The Convertible Reserved Instance offering ID.
    offeringId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TargetConfigurationRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceCount', 'targetConfigurationRequest_instanceCount' - The number of instances the Convertible Reserved Instance offering can
-- be applied to. This parameter is reserved and cannot be specified in a
-- request
--
-- 'offeringId', 'targetConfigurationRequest_offeringId' - The Convertible Reserved Instance offering ID.
newTargetConfigurationRequest ::
  -- | 'offeringId'
  Prelude.Text ->
  TargetConfigurationRequest
newTargetConfigurationRequest pOfferingId_ =
  TargetConfigurationRequest'
    { instanceCount =
        Prelude.Nothing,
      offeringId = pOfferingId_
    }

-- | The number of instances the Convertible Reserved Instance offering can
-- be applied to. This parameter is reserved and cannot be specified in a
-- request
targetConfigurationRequest_instanceCount :: Lens.Lens' TargetConfigurationRequest (Prelude.Maybe Prelude.Int)
targetConfigurationRequest_instanceCount = Lens.lens (\TargetConfigurationRequest' {instanceCount} -> instanceCount) (\s@TargetConfigurationRequest' {} a -> s {instanceCount = a} :: TargetConfigurationRequest)

-- | The Convertible Reserved Instance offering ID.
targetConfigurationRequest_offeringId :: Lens.Lens' TargetConfigurationRequest Prelude.Text
targetConfigurationRequest_offeringId = Lens.lens (\TargetConfigurationRequest' {offeringId} -> offeringId) (\s@TargetConfigurationRequest' {} a -> s {offeringId = a} :: TargetConfigurationRequest)

instance Prelude.Hashable TargetConfigurationRequest where
  hashWithSalt _salt TargetConfigurationRequest' {..} =
    _salt
      `Prelude.hashWithSalt` instanceCount
      `Prelude.hashWithSalt` offeringId

instance Prelude.NFData TargetConfigurationRequest where
  rnf TargetConfigurationRequest' {..} =
    Prelude.rnf instanceCount
      `Prelude.seq` Prelude.rnf offeringId

instance Data.ToQuery TargetConfigurationRequest where
  toQuery TargetConfigurationRequest' {..} =
    Prelude.mconcat
      [ "InstanceCount" Data.=: instanceCount,
        "OfferingId" Data.=: offeringId
      ]
