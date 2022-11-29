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
-- Module      : Amazonka.IoTEvents.Types.InputSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTEvents.Types.InputSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoTEvents.Types.InputStatus
import qualified Amazonka.Prelude as Prelude

-- | Information about the input.
--
-- /See:/ 'newInputSummary' smart constructor.
data InputSummary = InputSummary'
  { -- | The name of the input.
    inputName :: Prelude.Maybe Prelude.Text,
    -- | The status of the input.
    status :: Prelude.Maybe InputStatus,
    -- | The time the input was created.
    creationTime :: Prelude.Maybe Core.POSIX,
    -- | The last time the input was updated.
    lastUpdateTime :: Prelude.Maybe Core.POSIX,
    -- | A brief description of the input.
    inputDescription :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the input.
    inputArn :: Prelude.Maybe Prelude.Text
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
-- 'inputName', 'inputSummary_inputName' - The name of the input.
--
-- 'status', 'inputSummary_status' - The status of the input.
--
-- 'creationTime', 'inputSummary_creationTime' - The time the input was created.
--
-- 'lastUpdateTime', 'inputSummary_lastUpdateTime' - The last time the input was updated.
--
-- 'inputDescription', 'inputSummary_inputDescription' - A brief description of the input.
--
-- 'inputArn', 'inputSummary_inputArn' - The ARN of the input.
newInputSummary ::
  InputSummary
newInputSummary =
  InputSummary'
    { inputName = Prelude.Nothing,
      status = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      lastUpdateTime = Prelude.Nothing,
      inputDescription = Prelude.Nothing,
      inputArn = Prelude.Nothing
    }

-- | The name of the input.
inputSummary_inputName :: Lens.Lens' InputSummary (Prelude.Maybe Prelude.Text)
inputSummary_inputName = Lens.lens (\InputSummary' {inputName} -> inputName) (\s@InputSummary' {} a -> s {inputName = a} :: InputSummary)

-- | The status of the input.
inputSummary_status :: Lens.Lens' InputSummary (Prelude.Maybe InputStatus)
inputSummary_status = Lens.lens (\InputSummary' {status} -> status) (\s@InputSummary' {} a -> s {status = a} :: InputSummary)

-- | The time the input was created.
inputSummary_creationTime :: Lens.Lens' InputSummary (Prelude.Maybe Prelude.UTCTime)
inputSummary_creationTime = Lens.lens (\InputSummary' {creationTime} -> creationTime) (\s@InputSummary' {} a -> s {creationTime = a} :: InputSummary) Prelude.. Lens.mapping Core._Time

-- | The last time the input was updated.
inputSummary_lastUpdateTime :: Lens.Lens' InputSummary (Prelude.Maybe Prelude.UTCTime)
inputSummary_lastUpdateTime = Lens.lens (\InputSummary' {lastUpdateTime} -> lastUpdateTime) (\s@InputSummary' {} a -> s {lastUpdateTime = a} :: InputSummary) Prelude.. Lens.mapping Core._Time

-- | A brief description of the input.
inputSummary_inputDescription :: Lens.Lens' InputSummary (Prelude.Maybe Prelude.Text)
inputSummary_inputDescription = Lens.lens (\InputSummary' {inputDescription} -> inputDescription) (\s@InputSummary' {} a -> s {inputDescription = a} :: InputSummary)

-- | The ARN of the input.
inputSummary_inputArn :: Lens.Lens' InputSummary (Prelude.Maybe Prelude.Text)
inputSummary_inputArn = Lens.lens (\InputSummary' {inputArn} -> inputArn) (\s@InputSummary' {} a -> s {inputArn = a} :: InputSummary)

instance Core.FromJSON InputSummary where
  parseJSON =
    Core.withObject
      "InputSummary"
      ( \x ->
          InputSummary'
            Prelude.<$> (x Core..:? "inputName")
            Prelude.<*> (x Core..:? "status")
            Prelude.<*> (x Core..:? "creationTime")
            Prelude.<*> (x Core..:? "lastUpdateTime")
            Prelude.<*> (x Core..:? "inputDescription")
            Prelude.<*> (x Core..:? "inputArn")
      )

instance Prelude.Hashable InputSummary where
  hashWithSalt _salt InputSummary' {..} =
    _salt `Prelude.hashWithSalt` inputName
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` lastUpdateTime
      `Prelude.hashWithSalt` inputDescription
      `Prelude.hashWithSalt` inputArn

instance Prelude.NFData InputSummary where
  rnf InputSummary' {..} =
    Prelude.rnf inputName
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf lastUpdateTime
      `Prelude.seq` Prelude.rnf inputDescription
      `Prelude.seq` Prelude.rnf inputArn
