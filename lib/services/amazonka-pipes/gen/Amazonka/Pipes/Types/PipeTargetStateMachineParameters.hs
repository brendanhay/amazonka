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
-- Module      : Amazonka.Pipes.Types.PipeTargetStateMachineParameters
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pipes.Types.PipeTargetStateMachineParameters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Pipes.Types.PipeTargetInvocationType
import qualified Amazonka.Prelude as Prelude

-- | The parameters for using a Step Functions state machine as a target.
--
-- /See:/ 'newPipeTargetStateMachineParameters' smart constructor.
data PipeTargetStateMachineParameters = PipeTargetStateMachineParameters'
  { -- | Specify whether to wait for the state machine to finish or not.
    invocationType :: Prelude.Maybe PipeTargetInvocationType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PipeTargetStateMachineParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'invocationType', 'pipeTargetStateMachineParameters_invocationType' - Specify whether to wait for the state machine to finish or not.
newPipeTargetStateMachineParameters ::
  PipeTargetStateMachineParameters
newPipeTargetStateMachineParameters =
  PipeTargetStateMachineParameters'
    { invocationType =
        Prelude.Nothing
    }

-- | Specify whether to wait for the state machine to finish or not.
pipeTargetStateMachineParameters_invocationType :: Lens.Lens' PipeTargetStateMachineParameters (Prelude.Maybe PipeTargetInvocationType)
pipeTargetStateMachineParameters_invocationType = Lens.lens (\PipeTargetStateMachineParameters' {invocationType} -> invocationType) (\s@PipeTargetStateMachineParameters' {} a -> s {invocationType = a} :: PipeTargetStateMachineParameters)

instance
  Data.FromJSON
    PipeTargetStateMachineParameters
  where
  parseJSON =
    Data.withObject
      "PipeTargetStateMachineParameters"
      ( \x ->
          PipeTargetStateMachineParameters'
            Prelude.<$> (x Data..:? "InvocationType")
      )

instance
  Prelude.Hashable
    PipeTargetStateMachineParameters
  where
  hashWithSalt
    _salt
    PipeTargetStateMachineParameters' {..} =
      _salt `Prelude.hashWithSalt` invocationType

instance
  Prelude.NFData
    PipeTargetStateMachineParameters
  where
  rnf PipeTargetStateMachineParameters' {..} =
    Prelude.rnf invocationType

instance Data.ToJSON PipeTargetStateMachineParameters where
  toJSON PipeTargetStateMachineParameters' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("InvocationType" Data..=)
              Prelude.<$> invocationType
          ]
      )
