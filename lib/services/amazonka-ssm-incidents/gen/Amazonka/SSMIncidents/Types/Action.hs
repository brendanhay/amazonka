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
-- Module      : Amazonka.SSMIncidents.Types.Action
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSMIncidents.Types.Action where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SSMIncidents.Types.SsmAutomation

-- | The action that starts at the beginning of an incident. The response
-- plan defines the action.
--
-- /See:/ 'newAction' smart constructor.
data Action = Action'
  { -- | The Systems Manager automation document to start as the runbook at the
    -- beginning of the incident.
    ssmAutomation :: Prelude.Maybe SsmAutomation
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Action' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ssmAutomation', 'action_ssmAutomation' - The Systems Manager automation document to start as the runbook at the
-- beginning of the incident.
newAction ::
  Action
newAction = Action' {ssmAutomation = Prelude.Nothing}

-- | The Systems Manager automation document to start as the runbook at the
-- beginning of the incident.
action_ssmAutomation :: Lens.Lens' Action (Prelude.Maybe SsmAutomation)
action_ssmAutomation = Lens.lens (\Action' {ssmAutomation} -> ssmAutomation) (\s@Action' {} a -> s {ssmAutomation = a} :: Action)

instance Data.FromJSON Action where
  parseJSON =
    Data.withObject
      "Action"
      ( \x ->
          Action' Prelude.<$> (x Data..:? "ssmAutomation")
      )

instance Prelude.Hashable Action where
  hashWithSalt _salt Action' {..} =
    _salt `Prelude.hashWithSalt` ssmAutomation

instance Prelude.NFData Action where
  rnf Action' {..} = Prelude.rnf ssmAutomation

instance Data.ToJSON Action where
  toJSON Action' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ssmAutomation" Data..=)
              Prelude.<$> ssmAutomation
          ]
      )
