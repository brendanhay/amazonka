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
-- Module      : Amazonka.OpenSearch.Types.VPCDerivedInfoStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpenSearch.Types.VPCDerivedInfoStatus where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.OpenSearch.Types.OptionStatus
import Amazonka.OpenSearch.Types.VPCDerivedInfo
import qualified Amazonka.Prelude as Prelude

-- | Status of the VPC options for a specified domain.
--
-- /See:/ 'newVPCDerivedInfoStatus' smart constructor.
data VPCDerivedInfoStatus = VPCDerivedInfoStatus'
  { -- | The VPC options for the specified domain.
    options :: VPCDerivedInfo,
    -- | The status of the VPC options for the specified domain.
    status :: OptionStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VPCDerivedInfoStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'options', 'vPCDerivedInfoStatus_options' - The VPC options for the specified domain.
--
-- 'status', 'vPCDerivedInfoStatus_status' - The status of the VPC options for the specified domain.
newVPCDerivedInfoStatus ::
  -- | 'options'
  VPCDerivedInfo ->
  -- | 'status'
  OptionStatus ->
  VPCDerivedInfoStatus
newVPCDerivedInfoStatus pOptions_ pStatus_ =
  VPCDerivedInfoStatus'
    { options = pOptions_,
      status = pStatus_
    }

-- | The VPC options for the specified domain.
vPCDerivedInfoStatus_options :: Lens.Lens' VPCDerivedInfoStatus VPCDerivedInfo
vPCDerivedInfoStatus_options = Lens.lens (\VPCDerivedInfoStatus' {options} -> options) (\s@VPCDerivedInfoStatus' {} a -> s {options = a} :: VPCDerivedInfoStatus)

-- | The status of the VPC options for the specified domain.
vPCDerivedInfoStatus_status :: Lens.Lens' VPCDerivedInfoStatus OptionStatus
vPCDerivedInfoStatus_status = Lens.lens (\VPCDerivedInfoStatus' {status} -> status) (\s@VPCDerivedInfoStatus' {} a -> s {status = a} :: VPCDerivedInfoStatus)

instance Core.FromJSON VPCDerivedInfoStatus where
  parseJSON =
    Core.withObject
      "VPCDerivedInfoStatus"
      ( \x ->
          VPCDerivedInfoStatus'
            Prelude.<$> (x Core..: "Options")
            Prelude.<*> (x Core..: "Status")
      )

instance Prelude.Hashable VPCDerivedInfoStatus where
  hashWithSalt _salt VPCDerivedInfoStatus' {..} =
    _salt `Prelude.hashWithSalt` options
      `Prelude.hashWithSalt` status

instance Prelude.NFData VPCDerivedInfoStatus where
  rnf VPCDerivedInfoStatus' {..} =
    Prelude.rnf options
      `Prelude.seq` Prelude.rnf status
