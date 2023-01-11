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
-- Module      : Amazonka.SMS.Types.AppValidationOutput
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SMS.Types.AppValidationOutput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SMS.Types.SSMOutput

-- | Output from validating an application.
--
-- /See:/ 'newAppValidationOutput' smart constructor.
data AppValidationOutput = AppValidationOutput'
  { -- | Output from using SSM to validate the application.
    ssmOutput :: Prelude.Maybe SSMOutput
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AppValidationOutput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ssmOutput', 'appValidationOutput_ssmOutput' - Output from using SSM to validate the application.
newAppValidationOutput ::
  AppValidationOutput
newAppValidationOutput =
  AppValidationOutput' {ssmOutput = Prelude.Nothing}

-- | Output from using SSM to validate the application.
appValidationOutput_ssmOutput :: Lens.Lens' AppValidationOutput (Prelude.Maybe SSMOutput)
appValidationOutput_ssmOutput = Lens.lens (\AppValidationOutput' {ssmOutput} -> ssmOutput) (\s@AppValidationOutput' {} a -> s {ssmOutput = a} :: AppValidationOutput)

instance Data.FromJSON AppValidationOutput where
  parseJSON =
    Data.withObject
      "AppValidationOutput"
      ( \x ->
          AppValidationOutput'
            Prelude.<$> (x Data..:? "ssmOutput")
      )

instance Prelude.Hashable AppValidationOutput where
  hashWithSalt _salt AppValidationOutput' {..} =
    _salt `Prelude.hashWithSalt` ssmOutput

instance Prelude.NFData AppValidationOutput where
  rnf AppValidationOutput' {..} = Prelude.rnf ssmOutput
