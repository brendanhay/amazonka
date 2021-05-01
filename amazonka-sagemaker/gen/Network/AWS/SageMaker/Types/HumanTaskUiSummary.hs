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
-- Module      : Network.AWS.SageMaker.Types.HumanTaskUiSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.HumanTaskUiSummary where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Container for human task user interface information.
--
-- /See:/ 'newHumanTaskUiSummary' smart constructor.
data HumanTaskUiSummary = HumanTaskUiSummary'
  { -- | The name of the human task user interface.
    humanTaskUiName :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the human task user interface.
    humanTaskUiArn :: Prelude.Text,
    -- | A timestamp when SageMaker created the human task user interface.
    creationTime :: Prelude.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        creationTime = Prelude._Time Lens.# pCreationTime_
      }

-- | The name of the human task user interface.
humanTaskUiSummary_humanTaskUiName :: Lens.Lens' HumanTaskUiSummary Prelude.Text
humanTaskUiSummary_humanTaskUiName = Lens.lens (\HumanTaskUiSummary' {humanTaskUiName} -> humanTaskUiName) (\s@HumanTaskUiSummary' {} a -> s {humanTaskUiName = a} :: HumanTaskUiSummary)

-- | The Amazon Resource Name (ARN) of the human task user interface.
humanTaskUiSummary_humanTaskUiArn :: Lens.Lens' HumanTaskUiSummary Prelude.Text
humanTaskUiSummary_humanTaskUiArn = Lens.lens (\HumanTaskUiSummary' {humanTaskUiArn} -> humanTaskUiArn) (\s@HumanTaskUiSummary' {} a -> s {humanTaskUiArn = a} :: HumanTaskUiSummary)

-- | A timestamp when SageMaker created the human task user interface.
humanTaskUiSummary_creationTime :: Lens.Lens' HumanTaskUiSummary Prelude.UTCTime
humanTaskUiSummary_creationTime = Lens.lens (\HumanTaskUiSummary' {creationTime} -> creationTime) (\s@HumanTaskUiSummary' {} a -> s {creationTime = a} :: HumanTaskUiSummary) Prelude.. Prelude._Time

instance Prelude.FromJSON HumanTaskUiSummary where
  parseJSON =
    Prelude.withObject
      "HumanTaskUiSummary"
      ( \x ->
          HumanTaskUiSummary'
            Prelude.<$> (x Prelude..: "HumanTaskUiName")
            Prelude.<*> (x Prelude..: "HumanTaskUiArn")
            Prelude.<*> (x Prelude..: "CreationTime")
      )

instance Prelude.Hashable HumanTaskUiSummary

instance Prelude.NFData HumanTaskUiSummary
