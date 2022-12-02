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
-- Module      : Amazonka.LexV2Models.Types.DialogCodeHookInvocationSetting
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.DialogCodeHookInvocationSetting where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types.PostDialogCodeHookInvocationSpecification
import qualified Amazonka.Prelude as Prelude

-- | Settings that specify the dialog code hook that is called by Amazon Lex
-- at a step of the conversation.
--
-- /See:/ 'newDialogCodeHookInvocationSetting' smart constructor.
data DialogCodeHookInvocationSetting = DialogCodeHookInvocationSetting'
  { -- | A label that indicates the dialog step from which the dialog code hook
    -- is happening.
    invocationLabel :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether a Lambda function should be invoked for the dialog.
    enableCodeHookInvocation :: Prelude.Bool,
    -- | Determines whether a dialog code hook is used when the intent is
    -- activated.
    active :: Prelude.Bool,
    -- | Contains the responses and actions that Amazon Lex takes after the
    -- Lambda function is complete.
    postCodeHookSpecification :: PostDialogCodeHookInvocationSpecification
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DialogCodeHookInvocationSetting' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'invocationLabel', 'dialogCodeHookInvocationSetting_invocationLabel' - A label that indicates the dialog step from which the dialog code hook
-- is happening.
--
-- 'enableCodeHookInvocation', 'dialogCodeHookInvocationSetting_enableCodeHookInvocation' - Indicates whether a Lambda function should be invoked for the dialog.
--
-- 'active', 'dialogCodeHookInvocationSetting_active' - Determines whether a dialog code hook is used when the intent is
-- activated.
--
-- 'postCodeHookSpecification', 'dialogCodeHookInvocationSetting_postCodeHookSpecification' - Contains the responses and actions that Amazon Lex takes after the
-- Lambda function is complete.
newDialogCodeHookInvocationSetting ::
  -- | 'enableCodeHookInvocation'
  Prelude.Bool ->
  -- | 'active'
  Prelude.Bool ->
  -- | 'postCodeHookSpecification'
  PostDialogCodeHookInvocationSpecification ->
  DialogCodeHookInvocationSetting
newDialogCodeHookInvocationSetting
  pEnableCodeHookInvocation_
  pActive_
  pPostCodeHookSpecification_ =
    DialogCodeHookInvocationSetting'
      { invocationLabel =
          Prelude.Nothing,
        enableCodeHookInvocation =
          pEnableCodeHookInvocation_,
        active = pActive_,
        postCodeHookSpecification =
          pPostCodeHookSpecification_
      }

-- | A label that indicates the dialog step from which the dialog code hook
-- is happening.
dialogCodeHookInvocationSetting_invocationLabel :: Lens.Lens' DialogCodeHookInvocationSetting (Prelude.Maybe Prelude.Text)
dialogCodeHookInvocationSetting_invocationLabel = Lens.lens (\DialogCodeHookInvocationSetting' {invocationLabel} -> invocationLabel) (\s@DialogCodeHookInvocationSetting' {} a -> s {invocationLabel = a} :: DialogCodeHookInvocationSetting)

-- | Indicates whether a Lambda function should be invoked for the dialog.
dialogCodeHookInvocationSetting_enableCodeHookInvocation :: Lens.Lens' DialogCodeHookInvocationSetting Prelude.Bool
dialogCodeHookInvocationSetting_enableCodeHookInvocation = Lens.lens (\DialogCodeHookInvocationSetting' {enableCodeHookInvocation} -> enableCodeHookInvocation) (\s@DialogCodeHookInvocationSetting' {} a -> s {enableCodeHookInvocation = a} :: DialogCodeHookInvocationSetting)

-- | Determines whether a dialog code hook is used when the intent is
-- activated.
dialogCodeHookInvocationSetting_active :: Lens.Lens' DialogCodeHookInvocationSetting Prelude.Bool
dialogCodeHookInvocationSetting_active = Lens.lens (\DialogCodeHookInvocationSetting' {active} -> active) (\s@DialogCodeHookInvocationSetting' {} a -> s {active = a} :: DialogCodeHookInvocationSetting)

-- | Contains the responses and actions that Amazon Lex takes after the
-- Lambda function is complete.
dialogCodeHookInvocationSetting_postCodeHookSpecification :: Lens.Lens' DialogCodeHookInvocationSetting PostDialogCodeHookInvocationSpecification
dialogCodeHookInvocationSetting_postCodeHookSpecification = Lens.lens (\DialogCodeHookInvocationSetting' {postCodeHookSpecification} -> postCodeHookSpecification) (\s@DialogCodeHookInvocationSetting' {} a -> s {postCodeHookSpecification = a} :: DialogCodeHookInvocationSetting)

instance
  Data.FromJSON
    DialogCodeHookInvocationSetting
  where
  parseJSON =
    Data.withObject
      "DialogCodeHookInvocationSetting"
      ( \x ->
          DialogCodeHookInvocationSetting'
            Prelude.<$> (x Data..:? "invocationLabel")
            Prelude.<*> (x Data..: "enableCodeHookInvocation")
            Prelude.<*> (x Data..: "active")
            Prelude.<*> (x Data..: "postCodeHookSpecification")
      )

instance
  Prelude.Hashable
    DialogCodeHookInvocationSetting
  where
  hashWithSalt
    _salt
    DialogCodeHookInvocationSetting' {..} =
      _salt `Prelude.hashWithSalt` invocationLabel
        `Prelude.hashWithSalt` enableCodeHookInvocation
        `Prelude.hashWithSalt` active
        `Prelude.hashWithSalt` postCodeHookSpecification

instance
  Prelude.NFData
    DialogCodeHookInvocationSetting
  where
  rnf DialogCodeHookInvocationSetting' {..} =
    Prelude.rnf invocationLabel
      `Prelude.seq` Prelude.rnf enableCodeHookInvocation
      `Prelude.seq` Prelude.rnf active
      `Prelude.seq` Prelude.rnf postCodeHookSpecification

instance Data.ToJSON DialogCodeHookInvocationSetting where
  toJSON DialogCodeHookInvocationSetting' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("invocationLabel" Data..=)
              Prelude.<$> invocationLabel,
            Prelude.Just
              ( "enableCodeHookInvocation"
                  Data..= enableCodeHookInvocation
              ),
            Prelude.Just ("active" Data..= active),
            Prelude.Just
              ( "postCodeHookSpecification"
                  Data..= postCodeHookSpecification
              )
          ]
      )
