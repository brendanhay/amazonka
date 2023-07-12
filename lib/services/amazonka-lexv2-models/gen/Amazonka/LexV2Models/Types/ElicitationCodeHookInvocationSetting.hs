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
-- Module      : Amazonka.LexV2Models.Types.ElicitationCodeHookInvocationSetting
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.ElicitationCodeHookInvocationSetting where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Settings that specify the dialog code hook that is called by Amazon Lex
-- between eliciting slot values.
--
-- /See:/ 'newElicitationCodeHookInvocationSetting' smart constructor.
data ElicitationCodeHookInvocationSetting = ElicitationCodeHookInvocationSetting'
  { -- | A label that indicates the dialog step from which the dialog code hook
    -- is happening.
    invocationLabel :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether a Lambda function should be invoked for the dialog.
    enableCodeHookInvocation :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ElicitationCodeHookInvocationSetting' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'invocationLabel', 'elicitationCodeHookInvocationSetting_invocationLabel' - A label that indicates the dialog step from which the dialog code hook
-- is happening.
--
-- 'enableCodeHookInvocation', 'elicitationCodeHookInvocationSetting_enableCodeHookInvocation' - Indicates whether a Lambda function should be invoked for the dialog.
newElicitationCodeHookInvocationSetting ::
  -- | 'enableCodeHookInvocation'
  Prelude.Bool ->
  ElicitationCodeHookInvocationSetting
newElicitationCodeHookInvocationSetting
  pEnableCodeHookInvocation_ =
    ElicitationCodeHookInvocationSetting'
      { invocationLabel =
          Prelude.Nothing,
        enableCodeHookInvocation =
          pEnableCodeHookInvocation_
      }

-- | A label that indicates the dialog step from which the dialog code hook
-- is happening.
elicitationCodeHookInvocationSetting_invocationLabel :: Lens.Lens' ElicitationCodeHookInvocationSetting (Prelude.Maybe Prelude.Text)
elicitationCodeHookInvocationSetting_invocationLabel = Lens.lens (\ElicitationCodeHookInvocationSetting' {invocationLabel} -> invocationLabel) (\s@ElicitationCodeHookInvocationSetting' {} a -> s {invocationLabel = a} :: ElicitationCodeHookInvocationSetting)

-- | Indicates whether a Lambda function should be invoked for the dialog.
elicitationCodeHookInvocationSetting_enableCodeHookInvocation :: Lens.Lens' ElicitationCodeHookInvocationSetting Prelude.Bool
elicitationCodeHookInvocationSetting_enableCodeHookInvocation = Lens.lens (\ElicitationCodeHookInvocationSetting' {enableCodeHookInvocation} -> enableCodeHookInvocation) (\s@ElicitationCodeHookInvocationSetting' {} a -> s {enableCodeHookInvocation = a} :: ElicitationCodeHookInvocationSetting)

instance
  Data.FromJSON
    ElicitationCodeHookInvocationSetting
  where
  parseJSON =
    Data.withObject
      "ElicitationCodeHookInvocationSetting"
      ( \x ->
          ElicitationCodeHookInvocationSetting'
            Prelude.<$> (x Data..:? "invocationLabel")
            Prelude.<*> (x Data..: "enableCodeHookInvocation")
      )

instance
  Prelude.Hashable
    ElicitationCodeHookInvocationSetting
  where
  hashWithSalt
    _salt
    ElicitationCodeHookInvocationSetting' {..} =
      _salt
        `Prelude.hashWithSalt` invocationLabel
        `Prelude.hashWithSalt` enableCodeHookInvocation

instance
  Prelude.NFData
    ElicitationCodeHookInvocationSetting
  where
  rnf ElicitationCodeHookInvocationSetting' {..} =
    Prelude.rnf invocationLabel
      `Prelude.seq` Prelude.rnf enableCodeHookInvocation

instance
  Data.ToJSON
    ElicitationCodeHookInvocationSetting
  where
  toJSON ElicitationCodeHookInvocationSetting' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("invocationLabel" Data..=)
              Prelude.<$> invocationLabel,
            Prelude.Just
              ( "enableCodeHookInvocation"
                  Data..= enableCodeHookInvocation
              )
          ]
      )
