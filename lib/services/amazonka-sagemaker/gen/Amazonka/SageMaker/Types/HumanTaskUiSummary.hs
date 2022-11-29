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
-- Module      : Amazonka.SageMaker.Types.HumanTaskUiSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.HumanTaskUiSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Container for human task user interface information.
--
-- /See:/ 'newHumanTaskUiSummary' smart constructor.
data HumanTaskUiSummary = HumanTaskUiSummary'
  { -- | The name of the human task user interface.
    humanTaskUiName :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the human task user interface.
    humanTaskUiArn :: Prelude.Text,
    -- | A timestamp when SageMaker created the human task user interface.
    creationTime :: Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HumanTaskUiSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'humanTaskUiName', 'humanTaskUiSummary_humanTaskUiName' - The name of the human task user interface.
--
-- 'humanTaskUiArn', 'humanTaskUiSummary_humanTaskUiArn' - The Amazon Resource Name (ARN) of the human task user interface.
--
-- 'creationTime', 'humanTaskUiSummary_creationTime' - A timestamp when SageMaker created the human task user interface.
newHumanTaskUiSummary ::
  -- | 'humanTaskUiName'
  Prelude.Text ->
  -- | 'humanTaskUiArn'
  Prelude.Text ->
  -- | 'creationTime'
  Prelude.UTCTime ->
  HumanTaskUiSummary
newHumanTaskUiSummary
  pHumanTaskUiName_
  pHumanTaskUiArn_
  pCreationTime_ =
    HumanTaskUiSummary'
      { humanTaskUiName =
          pHumanTaskUiName_,
        humanTaskUiArn = pHumanTaskUiArn_,
        creationTime = Core._Time Lens.# pCreationTime_
      }

-- | The name of the human task user interface.
humanTaskUiSummary_humanTaskUiName :: Lens.Lens' HumanTaskUiSummary Prelude.Text
humanTaskUiSummary_humanTaskUiName = Lens.lens (\HumanTaskUiSummary' {humanTaskUiName} -> humanTaskUiName) (\s@HumanTaskUiSummary' {} a -> s {humanTaskUiName = a} :: HumanTaskUiSummary)

-- | The Amazon Resource Name (ARN) of the human task user interface.
humanTaskUiSummary_humanTaskUiArn :: Lens.Lens' HumanTaskUiSummary Prelude.Text
humanTaskUiSummary_humanTaskUiArn = Lens.lens (\HumanTaskUiSummary' {humanTaskUiArn} -> humanTaskUiArn) (\s@HumanTaskUiSummary' {} a -> s {humanTaskUiArn = a} :: HumanTaskUiSummary)

-- | A timestamp when SageMaker created the human task user interface.
humanTaskUiSummary_creationTime :: Lens.Lens' HumanTaskUiSummary Prelude.UTCTime
humanTaskUiSummary_creationTime = Lens.lens (\HumanTaskUiSummary' {creationTime} -> creationTime) (\s@HumanTaskUiSummary' {} a -> s {creationTime = a} :: HumanTaskUiSummary) Prelude.. Core._Time

instance Core.FromJSON HumanTaskUiSummary where
  parseJSON =
    Core.withObject
      "HumanTaskUiSummary"
      ( \x ->
          HumanTaskUiSummary'
            Prelude.<$> (x Core..: "HumanTaskUiName")
            Prelude.<*> (x Core..: "HumanTaskUiArn")
            Prelude.<*> (x Core..: "CreationTime")
      )

instance Prelude.Hashable HumanTaskUiSummary where
  hashWithSalt _salt HumanTaskUiSummary' {..} =
    _salt `Prelude.hashWithSalt` humanTaskUiName
      `Prelude.hashWithSalt` humanTaskUiArn
      `Prelude.hashWithSalt` creationTime

instance Prelude.NFData HumanTaskUiSummary where
  rnf HumanTaskUiSummary' {..} =
    Prelude.rnf humanTaskUiName
      `Prelude.seq` Prelude.rnf humanTaskUiArn
      `Prelude.seq` Prelude.rnf creationTime
