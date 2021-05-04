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
-- Module      : Network.AWS.SMS.Types.AppValidationOutput
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SMS.Types.AppValidationOutput where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SMS.Types.SSMOutput

-- | Output from validating an application.
--
-- /See:/ 'newAppValidationOutput' smart constructor.
data AppValidationOutput = AppValidationOutput'
  { -- | Output from using SSM to validate the application.
    ssmOutput :: Prelude.Maybe SSMOutput
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.FromJSON AppValidationOutput where
  parseJSON =
    Prelude.withObject
      "AppValidationOutput"
      ( \x ->
          AppValidationOutput'
            Prelude.<$> (x Prelude..:? "ssmOutput")
      )

instance Prelude.Hashable AppValidationOutput

instance Prelude.NFData AppValidationOutput
