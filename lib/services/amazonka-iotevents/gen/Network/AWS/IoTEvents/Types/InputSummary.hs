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
-- Module      : Network.AWS.IoTEvents.Types.InputSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTEvents.Types.InputSummary where

import qualified Network.AWS.Core as Core
import Network.AWS.IoTEvents.Types.InputStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about the input.
--
-- /See:/ 'newInputSummary' smart constructor.
data InputSummary = InputSummary'
  { -- | The time the input was created.
    creationTime :: Prelude.Maybe Core.POSIX,
    -- | The status of the input.
    status :: Prelude.Maybe InputStatus,
    -- | The name of the input.
    inputName :: Prelude.Maybe Prelude.Text,
    -- | A brief description of the input.
    inputDescription :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the input.
    inputArn :: Prelude.Maybe Prelude.Text,
    -- | The last time the input was updated.
    lastUpdateTime :: Prelude.Maybe Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InputSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'inputSummary_creationTime' - The time the input was created.
--
-- 'status', 'inputSummary_status' - The status of the input.
--
-- 'inputName', 'inputSummary_inputName' - The name of the input.
--
-- 'inputDescription', 'inputSummary_inputDescription' - A brief description of the input.
--
-- 'inputArn', 'inputSummary_inputArn' - The ARN of the input.
--
-- 'lastUpdateTime', 'inputSummary_lastUpdateTime' - The last time the input was updated.
newInputSummary ::
  InputSummary
newInputSummary =
  InputSummary'
    { creationTime = Prelude.Nothing,
      status = Prelude.Nothing,
      inputName = Prelude.Nothing,
      inputDescription = Prelude.Nothing,
      inputArn = Prelude.Nothing,
      lastUpdateTime = Prelude.Nothing
    }

-- | The time the input was created.
inputSummary_creationTime :: Lens.Lens' InputSummary (Prelude.Maybe Prelude.UTCTime)
inputSummary_creationTime = Lens.lens (\InputSummary' {creationTime} -> creationTime) (\s@InputSummary' {} a -> s {creationTime = a} :: InputSummary) Prelude.. Lens.mapping Core._Time

-- | The status of the input.
inputSummary_status :: Lens.Lens' InputSummary (Prelude.Maybe InputStatus)
inputSummary_status = Lens.lens (\InputSummary' {status} -> status) (\s@InputSummary' {} a -> s {status = a} :: InputSummary)

-- | The name of the input.
inputSummary_inputName :: Lens.Lens' InputSummary (Prelude.Maybe Prelude.Text)
inputSummary_inputName = Lens.lens (\InputSummary' {inputName} -> inputName) (\s@InputSummary' {} a -> s {inputName = a} :: InputSummary)

-- | A brief description of the input.
inputSummary_inputDescription :: Lens.Lens' InputSummary (Prelude.Maybe Prelude.Text)
inputSummary_inputDescription = Lens.lens (\InputSummary' {inputDescription} -> inputDescription) (\s@InputSummary' {} a -> s {inputDescription = a} :: InputSummary)

-- | The ARN of the input.
inputSummary_inputArn :: Lens.Lens' InputSummary (Prelude.Maybe Prelude.Text)
inputSummary_inputArn = Lens.lens (\InputSummary' {inputArn} -> inputArn) (\s@InputSummary' {} a -> s {inputArn = a} :: InputSummary)

-- | The last time the input was updated.
inputSummary_lastUpdateTime :: Lens.Lens' InputSummary (Prelude.Maybe Prelude.UTCTime)
inputSummary_lastUpdateTime = Lens.lens (\InputSummary' {lastUpdateTime} -> lastUpdateTime) (\s@InputSummary' {} a -> s {lastUpdateTime = a} :: InputSummary) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON InputSummary where
  parseJSON =
    Core.withObject
      "InputSummary"
      ( \x ->
          InputSummary'
            Prelude.<$> (x Core..:? "creationTime")
            Prelude.<*> (x Core..:? "status")
            Prelude.<*> (x Core..:? "inputName")
            Prelude.<*> (x Core..:? "inputDescription")
            Prelude.<*> (x Core..:? "inputArn")
            Prelude.<*> (x Core..:? "lastUpdateTime")
      )

instance Prelude.Hashable InputSummary

instance Prelude.NFData InputSummary
