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
-- Module      : Network.AWS.SageMaker.Types.HumanTaskUiSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.HumanTaskUiSummary where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Container for human task user interface information.
--
-- /See:/ 'newHumanTaskUiSummary' smart constructor.
data HumanTaskUiSummary = HumanTaskUiSummary'
  { -- | The name of the human task user interface.
    humanTaskUiName :: Core.Text,
    -- | The Amazon Resource Name (ARN) of the human task user interface.
    humanTaskUiArn :: Core.Text,
    -- | A timestamp when SageMaker created the human task user interface.
    creationTime :: Core.POSIX
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  -- | 'humanTaskUiArn'
  Core.Text ->
  -- | 'creationTime'
  Core.UTCTime ->
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
humanTaskUiSummary_humanTaskUiName :: Lens.Lens' HumanTaskUiSummary Core.Text
humanTaskUiSummary_humanTaskUiName = Lens.lens (\HumanTaskUiSummary' {humanTaskUiName} -> humanTaskUiName) (\s@HumanTaskUiSummary' {} a -> s {humanTaskUiName = a} :: HumanTaskUiSummary)

-- | The Amazon Resource Name (ARN) of the human task user interface.
humanTaskUiSummary_humanTaskUiArn :: Lens.Lens' HumanTaskUiSummary Core.Text
humanTaskUiSummary_humanTaskUiArn = Lens.lens (\HumanTaskUiSummary' {humanTaskUiArn} -> humanTaskUiArn) (\s@HumanTaskUiSummary' {} a -> s {humanTaskUiArn = a} :: HumanTaskUiSummary)

-- | A timestamp when SageMaker created the human task user interface.
humanTaskUiSummary_creationTime :: Lens.Lens' HumanTaskUiSummary Core.UTCTime
humanTaskUiSummary_creationTime = Lens.lens (\HumanTaskUiSummary' {creationTime} -> creationTime) (\s@HumanTaskUiSummary' {} a -> s {creationTime = a} :: HumanTaskUiSummary) Core.. Core._Time

instance Core.FromJSON HumanTaskUiSummary where
  parseJSON =
    Core.withObject
      "HumanTaskUiSummary"
      ( \x ->
          HumanTaskUiSummary'
            Core.<$> (x Core..: "HumanTaskUiName")
            Core.<*> (x Core..: "HumanTaskUiArn")
            Core.<*> (x Core..: "CreationTime")
      )

instance Core.Hashable HumanTaskUiSummary

instance Core.NFData HumanTaskUiSummary
