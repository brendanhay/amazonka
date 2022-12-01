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
  { initialResponse :: Prelude.Maybe ResponseSpecification,
    codeHook :: Prelude.Maybe DialogCodeHookInvocationSetting,
    -- | The next step in the conversation.
    nextStep :: Prelude.Maybe DialogState,
    conditional :: Prelude.Maybe ConditionalSpecification
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
-- 'initialResponse', 'initialResponseSetting_initialResponse' - Undocumented member.
--
-- 'codeHook', 'initialResponseSetting_codeHook' - Undocumented member.
--
-- 'nextStep', 'initialResponseSetting_nextStep' - The next step in the conversation.
--
-- 'conditional', 'initialResponseSetting_conditional' - Undocumented member.
newInitialResponseSetting ::
  InitialResponseSetting
newInitialResponseSetting =
  InitialResponseSetting'
    { initialResponse =
        Prelude.Nothing,
      codeHook = Prelude.Nothing,
      nextStep = Prelude.Nothing,
      conditional = Prelude.Nothing
    }

-- | Undocumented member.
initialResponseSetting_initialResponse :: Lens.Lens' InitialResponseSetting (Prelude.Maybe ResponseSpecification)
initialResponseSetting_initialResponse = Lens.lens (\InitialResponseSetting' {initialResponse} -> initialResponse) (\s@InitialResponseSetting' {} a -> s {initialResponse = a} :: InitialResponseSetting)

-- | Undocumented member.
initialResponseSetting_codeHook :: Lens.Lens' InitialResponseSetting (Prelude.Maybe DialogCodeHookInvocationSetting)
initialResponseSetting_codeHook = Lens.lens (\InitialResponseSetting' {codeHook} -> codeHook) (\s@InitialResponseSetting' {} a -> s {codeHook = a} :: InitialResponseSetting)

-- | The next step in the conversation.
initialResponseSetting_nextStep :: Lens.Lens' InitialResponseSetting (Prelude.Maybe DialogState)
initialResponseSetting_nextStep = Lens.lens (\InitialResponseSetting' {nextStep} -> nextStep) (\s@InitialResponseSetting' {} a -> s {nextStep = a} :: InitialResponseSetting)

-- | Undocumented member.
initialResponseSetting_conditional :: Lens.Lens' InitialResponseSetting (Prelude.Maybe ConditionalSpecification)
initialResponseSetting_conditional = Lens.lens (\InitialResponseSetting' {conditional} -> conditional) (\s@InitialResponseSetting' {} a -> s {conditional = a} :: InitialResponseSetting)

instance Core.FromJSON InitialResponseSetting where
  parseJSON =
    Core.withObject
      "InitialResponseSetting"
      ( \x ->
          InitialResponseSetting'
            Prelude.<$> (x Core..:? "initialResponse")
            Prelude.<*> (x Core..:? "codeHook")
            Prelude.<*> (x Core..:? "nextStep")
            Prelude.<*> (x Core..:? "conditional")
      )

instance Prelude.Hashable InitialResponseSetting where
  hashWithSalt _salt InitialResponseSetting' {..} =
    _salt `Prelude.hashWithSalt` initialResponse
      `Prelude.hashWithSalt` codeHook
      `Prelude.hashWithSalt` nextStep
      `Prelude.hashWithSalt` conditional

instance Prelude.NFData InitialResponseSetting where
  rnf InitialResponseSetting' {..} =
    Prelude.rnf initialResponse
      `Prelude.seq` Prelude.rnf codeHook
      `Prelude.seq` Prelude.rnf nextStep
      `Prelude.seq` Prelude.rnf conditional

instance Core.ToJSON InitialResponseSetting where
  toJSON InitialResponseSetting' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("initialResponse" Core..=)
              Prelude.<$> initialResponse,
            ("codeHook" Core..=) Prelude.<$> codeHook,
            ("nextStep" Core..=) Prelude.<$> nextStep,
            ("conditional" Core..=) Prelude.<$> conditional
          ]
      )
