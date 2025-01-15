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
-- Module      : Amazonka.SSMContacts.Types.Stage
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSMContacts.Types.Stage where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SSMContacts.Types.Target

-- | A set amount of time that an escalation plan or engagement plan engages
-- the specified contacts or contact methods.
--
-- /See:/ 'newStage' smart constructor.
data Stage = Stage'
  { -- | The time to wait until beginning the next stage. The duration can only
    -- be set to 0 if a target is specified.
    durationInMinutes :: Prelude.Natural,
    -- | The contacts or contact methods that the escalation plan or engagement
    -- plan is engaging.
    targets :: [Target]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Stage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'durationInMinutes', 'stage_durationInMinutes' - The time to wait until beginning the next stage. The duration can only
-- be set to 0 if a target is specified.
--
-- 'targets', 'stage_targets' - The contacts or contact methods that the escalation plan or engagement
-- plan is engaging.
newStage ::
  -- | 'durationInMinutes'
  Prelude.Natural ->
  Stage
newStage pDurationInMinutes_ =
  Stage'
    { durationInMinutes = pDurationInMinutes_,
      targets = Prelude.mempty
    }

-- | The time to wait until beginning the next stage. The duration can only
-- be set to 0 if a target is specified.
stage_durationInMinutes :: Lens.Lens' Stage Prelude.Natural
stage_durationInMinutes = Lens.lens (\Stage' {durationInMinutes} -> durationInMinutes) (\s@Stage' {} a -> s {durationInMinutes = a} :: Stage)

-- | The contacts or contact methods that the escalation plan or engagement
-- plan is engaging.
stage_targets :: Lens.Lens' Stage [Target]
stage_targets = Lens.lens (\Stage' {targets} -> targets) (\s@Stage' {} a -> s {targets = a} :: Stage) Prelude.. Lens.coerced

instance Data.FromJSON Stage where
  parseJSON =
    Data.withObject
      "Stage"
      ( \x ->
          Stage'
            Prelude.<$> (x Data..: "DurationInMinutes")
            Prelude.<*> (x Data..:? "Targets" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable Stage where
  hashWithSalt _salt Stage' {..} =
    _salt
      `Prelude.hashWithSalt` durationInMinutes
      `Prelude.hashWithSalt` targets

instance Prelude.NFData Stage where
  rnf Stage' {..} =
    Prelude.rnf durationInMinutes `Prelude.seq`
      Prelude.rnf targets

instance Data.ToJSON Stage where
  toJSON Stage' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("DurationInMinutes" Data..= durationInMinutes),
            Prelude.Just ("Targets" Data..= targets)
          ]
      )
