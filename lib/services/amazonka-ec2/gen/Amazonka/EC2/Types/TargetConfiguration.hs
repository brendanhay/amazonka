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
-- Module      : Amazonka.EC2.Types.TargetConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.TargetConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | Information about the Convertible Reserved Instance offering.
--
-- /See:/ 'newTargetConfiguration' smart constructor.
data TargetConfiguration = TargetConfiguration'
  { -- | The number of instances the Convertible Reserved Instance offering can
    -- be applied to. This parameter is reserved and cannot be specified in a
    -- request
    instanceCount :: Prelude.Maybe Prelude.Int,
    -- | The ID of the Convertible Reserved Instance offering.
    offeringId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TargetConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceCount', 'targetConfiguration_instanceCount' - The number of instances the Convertible Reserved Instance offering can
-- be applied to. This parameter is reserved and cannot be specified in a
-- request
--
-- 'offeringId', 'targetConfiguration_offeringId' - The ID of the Convertible Reserved Instance offering.
newTargetConfiguration ::
  TargetConfiguration
newTargetConfiguration =
  TargetConfiguration'
    { instanceCount =
        Prelude.Nothing,
      offeringId = Prelude.Nothing
    }

-- | The number of instances the Convertible Reserved Instance offering can
-- be applied to. This parameter is reserved and cannot be specified in a
-- request
targetConfiguration_instanceCount :: Lens.Lens' TargetConfiguration (Prelude.Maybe Prelude.Int)
targetConfiguration_instanceCount = Lens.lens (\TargetConfiguration' {instanceCount} -> instanceCount) (\s@TargetConfiguration' {} a -> s {instanceCount = a} :: TargetConfiguration)

-- | The ID of the Convertible Reserved Instance offering.
targetConfiguration_offeringId :: Lens.Lens' TargetConfiguration (Prelude.Maybe Prelude.Text)
targetConfiguration_offeringId = Lens.lens (\TargetConfiguration' {offeringId} -> offeringId) (\s@TargetConfiguration' {} a -> s {offeringId = a} :: TargetConfiguration)

instance Data.FromXML TargetConfiguration where
  parseXML x =
    TargetConfiguration'
      Prelude.<$> (x Data..@? "instanceCount")
      Prelude.<*> (x Data..@? "offeringId")

instance Prelude.Hashable TargetConfiguration where
  hashWithSalt _salt TargetConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` instanceCount
      `Prelude.hashWithSalt` offeringId

instance Prelude.NFData TargetConfiguration where
  rnf TargetConfiguration' {..} =
    Prelude.rnf instanceCount `Prelude.seq`
      Prelude.rnf offeringId
