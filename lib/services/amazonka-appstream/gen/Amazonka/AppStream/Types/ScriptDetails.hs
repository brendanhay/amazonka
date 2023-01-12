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
-- Module      : Amazonka.AppStream.Types.ScriptDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppStream.Types.ScriptDetails where

import Amazonka.AppStream.Types.S3Location
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the details of the script.
--
-- /See:/ 'newScriptDetails' smart constructor.
data ScriptDetails = ScriptDetails'
  { -- | The runtime parameters passed to the run path for the script.
    executableParameters :: Prelude.Maybe Prelude.Text,
    -- | The S3 object location for the script.
    scriptS3Location :: S3Location,
    -- | The run path for the script.
    executablePath :: Prelude.Text,
    -- | The run timeout, in seconds, for the script.
    timeoutInSeconds :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ScriptDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'executableParameters', 'scriptDetails_executableParameters' - The runtime parameters passed to the run path for the script.
--
-- 'scriptS3Location', 'scriptDetails_scriptS3Location' - The S3 object location for the script.
--
-- 'executablePath', 'scriptDetails_executablePath' - The run path for the script.
--
-- 'timeoutInSeconds', 'scriptDetails_timeoutInSeconds' - The run timeout, in seconds, for the script.
newScriptDetails ::
  -- | 'scriptS3Location'
  S3Location ->
  -- | 'executablePath'
  Prelude.Text ->
  -- | 'timeoutInSeconds'
  Prelude.Int ->
  ScriptDetails
newScriptDetails
  pScriptS3Location_
  pExecutablePath_
  pTimeoutInSeconds_ =
    ScriptDetails'
      { executableParameters =
          Prelude.Nothing,
        scriptS3Location = pScriptS3Location_,
        executablePath = pExecutablePath_,
        timeoutInSeconds = pTimeoutInSeconds_
      }

-- | The runtime parameters passed to the run path for the script.
scriptDetails_executableParameters :: Lens.Lens' ScriptDetails (Prelude.Maybe Prelude.Text)
scriptDetails_executableParameters = Lens.lens (\ScriptDetails' {executableParameters} -> executableParameters) (\s@ScriptDetails' {} a -> s {executableParameters = a} :: ScriptDetails)

-- | The S3 object location for the script.
scriptDetails_scriptS3Location :: Lens.Lens' ScriptDetails S3Location
scriptDetails_scriptS3Location = Lens.lens (\ScriptDetails' {scriptS3Location} -> scriptS3Location) (\s@ScriptDetails' {} a -> s {scriptS3Location = a} :: ScriptDetails)

-- | The run path for the script.
scriptDetails_executablePath :: Lens.Lens' ScriptDetails Prelude.Text
scriptDetails_executablePath = Lens.lens (\ScriptDetails' {executablePath} -> executablePath) (\s@ScriptDetails' {} a -> s {executablePath = a} :: ScriptDetails)

-- | The run timeout, in seconds, for the script.
scriptDetails_timeoutInSeconds :: Lens.Lens' ScriptDetails Prelude.Int
scriptDetails_timeoutInSeconds = Lens.lens (\ScriptDetails' {timeoutInSeconds} -> timeoutInSeconds) (\s@ScriptDetails' {} a -> s {timeoutInSeconds = a} :: ScriptDetails)

instance Data.FromJSON ScriptDetails where
  parseJSON =
    Data.withObject
      "ScriptDetails"
      ( \x ->
          ScriptDetails'
            Prelude.<$> (x Data..:? "ExecutableParameters")
            Prelude.<*> (x Data..: "ScriptS3Location")
            Prelude.<*> (x Data..: "ExecutablePath")
            Prelude.<*> (x Data..: "TimeoutInSeconds")
      )

instance Prelude.Hashable ScriptDetails where
  hashWithSalt _salt ScriptDetails' {..} =
    _salt `Prelude.hashWithSalt` executableParameters
      `Prelude.hashWithSalt` scriptS3Location
      `Prelude.hashWithSalt` executablePath
      `Prelude.hashWithSalt` timeoutInSeconds

instance Prelude.NFData ScriptDetails where
  rnf ScriptDetails' {..} =
    Prelude.rnf executableParameters
      `Prelude.seq` Prelude.rnf scriptS3Location
      `Prelude.seq` Prelude.rnf executablePath
      `Prelude.seq` Prelude.rnf timeoutInSeconds

instance Data.ToJSON ScriptDetails where
  toJSON ScriptDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ExecutableParameters" Data..=)
              Prelude.<$> executableParameters,
            Prelude.Just
              ("ScriptS3Location" Data..= scriptS3Location),
            Prelude.Just
              ("ExecutablePath" Data..= executablePath),
            Prelude.Just
              ("TimeoutInSeconds" Data..= timeoutInSeconds)
          ]
      )
