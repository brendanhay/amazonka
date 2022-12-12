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
-- Module      : Amazonka.LexV2Models.Types.InitialResponseSetting
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.InitialResponseSetting where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types.ConditionalSpecification
import Amazonka.LexV2Models.Types.DialogCodeHookInvocationSetting
import Amazonka.LexV2Models.Types.DialogState
import Amazonka.LexV2Models.Types.ResponseSpecification
import qualified Amazonka.Prelude as Prelude

-- | Configuration setting for a response sent to the user before Amazon Lex
-- starts eliciting slots.
--
-- /See:/ 'newInitialResponseSetting' smart constructor.
data InitialResponseSetting = InitialResponseSetting'
  { codeHook :: Prelude.Maybe DialogCodeHookInvocationSetting,
    conditional :: Prelude.Maybe ConditionalSpecification,
    initialResponse :: Prelude.Maybe ResponseSpecification,
    -- | The next step in the conversation.
    nextStep :: Prelude.Maybe DialogState
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InitialResponseSetting' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'codeHook', 'initialResponseSetting_codeHook' - Undocumented member.
--
-- 'conditional', 'initialResponseSetting_conditional' - Undocumented member.
--
-- 'initialResponse', 'initialResponseSetting_initialResponse' - Undocumented member.
--
-- 'nextStep', 'initialResponseSetting_nextStep' - The next step in the conversation.
newInitialResponseSetting ::
  InitialResponseSetting
newInitialResponseSetting =
  InitialResponseSetting'
    { codeHook = Prelude.Nothing,
      conditional = Prelude.Nothing,
      initialResponse = Prelude.Nothing,
      nextStep = Prelude.Nothing
    }

-- | Undocumented member.
initialResponseSetting_codeHook :: Lens.Lens' InitialResponseSetting (Prelude.Maybe DialogCodeHookInvocationSetting)
initialResponseSetting_codeHook = Lens.lens (\InitialResponseSetting' {codeHook} -> codeHook) (\s@InitialResponseSetting' {} a -> s {codeHook = a} :: InitialResponseSetting)

-- | Undocumented member.
initialResponseSetting_conditional :: Lens.Lens' InitialResponseSetting (Prelude.Maybe ConditionalSpecification)
initialResponseSetting_conditional = Lens.lens (\InitialResponseSetting' {conditional} -> conditional) (\s@InitialResponseSetting' {} a -> s {conditional = a} :: InitialResponseSetting)

-- | Undocumented member.
initialResponseSetting_initialResponse :: Lens.Lens' InitialResponseSetting (Prelude.Maybe ResponseSpecification)
initialResponseSetting_initialResponse = Lens.lens (\InitialResponseSetting' {initialResponse} -> initialResponse) (\s@InitialResponseSetting' {} a -> s {initialResponse = a} :: InitialResponseSetting)

-- | The next step in the conversation.
initialResponseSetting_nextStep :: Lens.Lens' InitialResponseSetting (Prelude.Maybe DialogState)
initialResponseSetting_nextStep = Lens.lens (\InitialResponseSetting' {nextStep} -> nextStep) (\s@InitialResponseSetting' {} a -> s {nextStep = a} :: InitialResponseSetting)

instance Data.FromJSON InitialResponseSetting where
  parseJSON =
    Data.withObject
      "InitialResponseSetting"
      ( \x ->
          InitialResponseSetting'
            Prelude.<$> (x Data..:? "codeHook")
            Prelude.<*> (x Data..:? "conditional")
            Prelude.<*> (x Data..:? "initialResponse")
            Prelude.<*> (x Data..:? "nextStep")
      )

instance Prelude.Hashable InitialResponseSetting where
  hashWithSalt _salt InitialResponseSetting' {..} =
    _salt `Prelude.hashWithSalt` codeHook
      `Prelude.hashWithSalt` conditional
      `Prelude.hashWithSalt` initialResponse
      `Prelude.hashWithSalt` nextStep

instance Prelude.NFData InitialResponseSetting where
  rnf InitialResponseSetting' {..} =
    Prelude.rnf codeHook
      `Prelude.seq` Prelude.rnf conditional
      `Prelude.seq` Prelude.rnf initialResponse
      `Prelude.seq` Prelude.rnf nextStep

instance Data.ToJSON InitialResponseSetting where
  toJSON InitialResponseSetting' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("codeHook" Data..=) Prelude.<$> codeHook,
            ("conditional" Data..=) Prelude.<$> conditional,
            ("initialResponse" Data..=)
              Prelude.<$> initialResponse,
            ("nextStep" Data..=) Prelude.<$> nextStep
          ]
      )
