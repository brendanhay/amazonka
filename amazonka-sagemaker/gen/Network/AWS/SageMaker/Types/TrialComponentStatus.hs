{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SageMaker.Types.TrialComponentPrimaryStatus

-- | The status of the trial component.
--
-- /See:/ 'newTrialComponentStatus' smart constructor.
data TrialComponentStatus = TrialComponentStatus'
  { -- | If the component failed, a message describing why.
    message :: Prelude.Maybe Prelude.Text,
    -- | The status of the trial component.
    primaryStatus :: Prelude.Maybe TrialComponentPrimaryStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { message = Prelude.Nothing,
      primaryStatus = Prelude.Nothing
    }

-- | If the component failed, a message describing why.
trialComponentStatus_message :: Lens.Lens' TrialComponentStatus (Prelude.Maybe Prelude.Text)
trialComponentStatus_message = Lens.lens (\TrialComponentStatus' {message} -> message) (\s@TrialComponentStatus' {} a -> s {message = a} :: TrialComponentStatus)

-- | The status of the trial component.
trialComponentStatus_primaryStatus :: Lens.Lens' TrialComponentStatus (Prelude.Maybe TrialComponentPrimaryStatus)
trialComponentStatus_primaryStatus = Lens.lens (\TrialComponentStatus' {primaryStatus} -> primaryStatus) (\s@TrialComponentStatus' {} a -> s {primaryStatus = a} :: TrialComponentStatus)

instance Prelude.FromJSON TrialComponentStatus where
  parseJSON =
    Prelude.withObject
      "TrialComponentStatus"
      ( \x ->
          TrialComponentStatus'
            Prelude.<$> (x Prelude..:? "Message")
            Prelude.<*> (x Prelude..:? "PrimaryStatus")
      )

instance Prelude.Hashable TrialComponentStatus

instance Prelude.NFData TrialComponentStatus

instance Prelude.ToJSON TrialComponentStatus where
  toJSON TrialComponentStatus' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("Message" Prelude..=) Prelude.<$> message,
            ("PrimaryStatus" Prelude..=)
              Prelude.<$> primaryStatus
          ]
      )
