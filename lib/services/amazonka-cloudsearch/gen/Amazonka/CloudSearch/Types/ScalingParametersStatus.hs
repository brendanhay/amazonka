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
-- Module      : Amazonka.CloudSearch.Types.ScalingParametersStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudSearch.Types.ScalingParametersStatus where

import Amazonka.CloudSearch.Types.OptionStatus
import Amazonka.CloudSearch.Types.ScalingParameters
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The status and configuration of a search domain\'s scaling parameters.
--
-- /See:/ 'newScalingParametersStatus' smart constructor.
data ScalingParametersStatus = ScalingParametersStatus'
  { options :: ScalingParameters,
    status :: OptionStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ScalingParametersStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'options', 'scalingParametersStatus_options' - Undocumented member.
--
-- 'status', 'scalingParametersStatus_status' - Undocumented member.
newScalingParametersStatus ::
  -- | 'options'
  ScalingParameters ->
  -- | 'status'
  OptionStatus ->
  ScalingParametersStatus
newScalingParametersStatus pOptions_ pStatus_ =
  ScalingParametersStatus'
    { options = pOptions_,
      status = pStatus_
    }

-- | Undocumented member.
scalingParametersStatus_options :: Lens.Lens' ScalingParametersStatus ScalingParameters
scalingParametersStatus_options = Lens.lens (\ScalingParametersStatus' {options} -> options) (\s@ScalingParametersStatus' {} a -> s {options = a} :: ScalingParametersStatus)

-- | Undocumented member.
scalingParametersStatus_status :: Lens.Lens' ScalingParametersStatus OptionStatus
scalingParametersStatus_status = Lens.lens (\ScalingParametersStatus' {status} -> status) (\s@ScalingParametersStatus' {} a -> s {status = a} :: ScalingParametersStatus)

instance Data.FromXML ScalingParametersStatus where
  parseXML x =
    ScalingParametersStatus'
      Prelude.<$> (x Data..@ "Options")
      Prelude.<*> (x Data..@ "Status")

instance Prelude.Hashable ScalingParametersStatus where
  hashWithSalt _salt ScalingParametersStatus' {..} =
    _salt `Prelude.hashWithSalt` options
      `Prelude.hashWithSalt` status

instance Prelude.NFData ScalingParametersStatus where
  rnf ScalingParametersStatus' {..} =
    Prelude.rnf options
      `Prelude.seq` Prelude.rnf status
