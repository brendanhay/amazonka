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
-- Module      : Amazonka.Inspector2.Types.EcrConfigurationState
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector2.Types.EcrConfigurationState where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Inspector2.Types.EcrRescanDurationState
import qualified Amazonka.Prelude as Prelude

-- | Details about the state of the ECR scans for your environment.
--
-- /See:/ 'newEcrConfigurationState' smart constructor.
data EcrConfigurationState = EcrConfigurationState'
  { -- | An object that contains details about the state of the ECR automated
    -- re-scan setting.
    rescanDurationState :: Prelude.Maybe EcrRescanDurationState
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EcrConfigurationState' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'rescanDurationState', 'ecrConfigurationState_rescanDurationState' - An object that contains details about the state of the ECR automated
-- re-scan setting.
newEcrConfigurationState ::
  EcrConfigurationState
newEcrConfigurationState =
  EcrConfigurationState'
    { rescanDurationState =
        Prelude.Nothing
    }

-- | An object that contains details about the state of the ECR automated
-- re-scan setting.
ecrConfigurationState_rescanDurationState :: Lens.Lens' EcrConfigurationState (Prelude.Maybe EcrRescanDurationState)
ecrConfigurationState_rescanDurationState = Lens.lens (\EcrConfigurationState' {rescanDurationState} -> rescanDurationState) (\s@EcrConfigurationState' {} a -> s {rescanDurationState = a} :: EcrConfigurationState)

instance Data.FromJSON EcrConfigurationState where
  parseJSON =
    Data.withObject
      "EcrConfigurationState"
      ( \x ->
          EcrConfigurationState'
            Prelude.<$> (x Data..:? "rescanDurationState")
      )

instance Prelude.Hashable EcrConfigurationState where
  hashWithSalt _salt EcrConfigurationState' {..} =
    _salt `Prelude.hashWithSalt` rescanDurationState

instance Prelude.NFData EcrConfigurationState where
  rnf EcrConfigurationState' {..} =
    Prelude.rnf rescanDurationState
