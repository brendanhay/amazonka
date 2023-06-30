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
-- Module      : Amazonka.IoTEvents.Types.InputConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTEvents.Types.InputConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTEvents.Types.InputStatus
import qualified Amazonka.Prelude as Prelude

-- | Information about the configuration of an input.
--
-- /See:/ 'newInputConfiguration' smart constructor.
data InputConfiguration = InputConfiguration'
  { -- | A brief description of the input.
    inputDescription :: Prelude.Maybe Prelude.Text,
    -- | The name of the input.
    inputName :: Prelude.Text,
    -- | The ARN of the input.
    inputArn :: Prelude.Text,
    -- | The time the input was created.
    creationTime :: Data.POSIX,
    -- | The last time the input was updated.
    lastUpdateTime :: Data.POSIX,
    -- | The status of the input.
    status :: InputStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InputConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'inputDescription', 'inputConfiguration_inputDescription' - A brief description of the input.
--
-- 'inputName', 'inputConfiguration_inputName' - The name of the input.
--
-- 'inputArn', 'inputConfiguration_inputArn' - The ARN of the input.
--
-- 'creationTime', 'inputConfiguration_creationTime' - The time the input was created.
--
-- 'lastUpdateTime', 'inputConfiguration_lastUpdateTime' - The last time the input was updated.
--
-- 'status', 'inputConfiguration_status' - The status of the input.
newInputConfiguration ::
  -- | 'inputName'
  Prelude.Text ->
  -- | 'inputArn'
  Prelude.Text ->
  -- | 'creationTime'
  Prelude.UTCTime ->
  -- | 'lastUpdateTime'
  Prelude.UTCTime ->
  -- | 'status'
  InputStatus ->
  InputConfiguration
newInputConfiguration
  pInputName_
  pInputArn_
  pCreationTime_
  pLastUpdateTime_
  pStatus_ =
    InputConfiguration'
      { inputDescription =
          Prelude.Nothing,
        inputName = pInputName_,
        inputArn = pInputArn_,
        creationTime = Data._Time Lens.# pCreationTime_,
        lastUpdateTime = Data._Time Lens.# pLastUpdateTime_,
        status = pStatus_
      }

-- | A brief description of the input.
inputConfiguration_inputDescription :: Lens.Lens' InputConfiguration (Prelude.Maybe Prelude.Text)
inputConfiguration_inputDescription = Lens.lens (\InputConfiguration' {inputDescription} -> inputDescription) (\s@InputConfiguration' {} a -> s {inputDescription = a} :: InputConfiguration)

-- | The name of the input.
inputConfiguration_inputName :: Lens.Lens' InputConfiguration Prelude.Text
inputConfiguration_inputName = Lens.lens (\InputConfiguration' {inputName} -> inputName) (\s@InputConfiguration' {} a -> s {inputName = a} :: InputConfiguration)

-- | The ARN of the input.
inputConfiguration_inputArn :: Lens.Lens' InputConfiguration Prelude.Text
inputConfiguration_inputArn = Lens.lens (\InputConfiguration' {inputArn} -> inputArn) (\s@InputConfiguration' {} a -> s {inputArn = a} :: InputConfiguration)

-- | The time the input was created.
inputConfiguration_creationTime :: Lens.Lens' InputConfiguration Prelude.UTCTime
inputConfiguration_creationTime = Lens.lens (\InputConfiguration' {creationTime} -> creationTime) (\s@InputConfiguration' {} a -> s {creationTime = a} :: InputConfiguration) Prelude.. Data._Time

-- | The last time the input was updated.
inputConfiguration_lastUpdateTime :: Lens.Lens' InputConfiguration Prelude.UTCTime
inputConfiguration_lastUpdateTime = Lens.lens (\InputConfiguration' {lastUpdateTime} -> lastUpdateTime) (\s@InputConfiguration' {} a -> s {lastUpdateTime = a} :: InputConfiguration) Prelude.. Data._Time

-- | The status of the input.
inputConfiguration_status :: Lens.Lens' InputConfiguration InputStatus
inputConfiguration_status = Lens.lens (\InputConfiguration' {status} -> status) (\s@InputConfiguration' {} a -> s {status = a} :: InputConfiguration)

instance Data.FromJSON InputConfiguration where
  parseJSON =
    Data.withObject
      "InputConfiguration"
      ( \x ->
          InputConfiguration'
            Prelude.<$> (x Data..:? "inputDescription")
            Prelude.<*> (x Data..: "inputName")
            Prelude.<*> (x Data..: "inputArn")
            Prelude.<*> (x Data..: "creationTime")
            Prelude.<*> (x Data..: "lastUpdateTime")
            Prelude.<*> (x Data..: "status")
      )

instance Prelude.Hashable InputConfiguration where
  hashWithSalt _salt InputConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` inputDescription
      `Prelude.hashWithSalt` inputName
      `Prelude.hashWithSalt` inputArn
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` lastUpdateTime
      `Prelude.hashWithSalt` status

instance Prelude.NFData InputConfiguration where
  rnf InputConfiguration' {..} =
    Prelude.rnf inputDescription
      `Prelude.seq` Prelude.rnf inputName
      `Prelude.seq` Prelude.rnf inputArn
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf lastUpdateTime
      `Prelude.seq` Prelude.rnf status
