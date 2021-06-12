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
-- Module      : Network.AWS.SageMaker.Types.TrialComponentStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.TrialComponentStatus where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SageMaker.Types.TrialComponentPrimaryStatus

-- | The status of the trial component.
--
-- /See:/ 'newTrialComponentStatus' smart constructor.
data TrialComponentStatus = TrialComponentStatus'
  { -- | If the component failed, a message describing why.
    message :: Core.Maybe Core.Text,
    -- | The status of the trial component.
    primaryStatus :: Core.Maybe TrialComponentPrimaryStatus
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'TrialComponentStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'message', 'trialComponentStatus_message' - If the component failed, a message describing why.
--
-- 'primaryStatus', 'trialComponentStatus_primaryStatus' - The status of the trial component.
newTrialComponentStatus ::
  TrialComponentStatus
newTrialComponentStatus =
  TrialComponentStatus'
    { message = Core.Nothing,
      primaryStatus = Core.Nothing
    }

-- | If the component failed, a message describing why.
trialComponentStatus_message :: Lens.Lens' TrialComponentStatus (Core.Maybe Core.Text)
trialComponentStatus_message = Lens.lens (\TrialComponentStatus' {message} -> message) (\s@TrialComponentStatus' {} a -> s {message = a} :: TrialComponentStatus)

-- | The status of the trial component.
trialComponentStatus_primaryStatus :: Lens.Lens' TrialComponentStatus (Core.Maybe TrialComponentPrimaryStatus)
trialComponentStatus_primaryStatus = Lens.lens (\TrialComponentStatus' {primaryStatus} -> primaryStatus) (\s@TrialComponentStatus' {} a -> s {primaryStatus = a} :: TrialComponentStatus)

instance Core.FromJSON TrialComponentStatus where
  parseJSON =
    Core.withObject
      "TrialComponentStatus"
      ( \x ->
          TrialComponentStatus'
            Core.<$> (x Core..:? "Message")
            Core.<*> (x Core..:? "PrimaryStatus")
      )

instance Core.Hashable TrialComponentStatus

instance Core.NFData TrialComponentStatus

instance Core.ToJSON TrialComponentStatus where
  toJSON TrialComponentStatus' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Message" Core..=) Core.<$> message,
            ("PrimaryStatus" Core..=) Core.<$> primaryStatus
          ]
      )
