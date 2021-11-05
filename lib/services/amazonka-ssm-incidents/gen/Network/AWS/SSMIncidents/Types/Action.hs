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
-- Module      : Network.AWS.SSMIncidents.Types.Action
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSMIncidents.Types.Action where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SSMIncidents.Types.SsmAutomation

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

instance Core.FromJSON Action where
  parseJSON =
    Core.withObject
      "Action"
      ( \x ->
          Action' Prelude.<$> (x Core..:? "ssmAutomation")
      )

instance Prelude.Hashable Action

instance Prelude.NFData Action

instance Core.ToJSON Action where
  toJSON Action' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ssmAutomation" Core..=)
              Prelude.<$> ssmAutomation
          ]
      )
