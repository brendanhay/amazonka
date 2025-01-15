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
-- Module      : Amazonka.LexModels.Types.FollowUpPrompt
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexModels.Types.FollowUpPrompt where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexModels.Types.Prompt
import Amazonka.LexModels.Types.Statement
import qualified Amazonka.Prelude as Prelude

-- | A prompt for additional activity after an intent is fulfilled. For
-- example, after the @OrderPizza@ intent is fulfilled, you might prompt
-- the user to find out whether the user wants to order drinks.
--
-- /See:/ 'newFollowUpPrompt' smart constructor.
data FollowUpPrompt = FollowUpPrompt'
  { -- | Prompts for information from the user.
    prompt :: Prompt,
    -- | If the user answers \"no\" to the question defined in the @prompt@
    -- field, Amazon Lex responds with this statement to acknowledge that the
    -- intent was canceled.
    rejectionStatement :: Statement
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FollowUpPrompt' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'prompt', 'followUpPrompt_prompt' - Prompts for information from the user.
--
-- 'rejectionStatement', 'followUpPrompt_rejectionStatement' - If the user answers \"no\" to the question defined in the @prompt@
-- field, Amazon Lex responds with this statement to acknowledge that the
-- intent was canceled.
newFollowUpPrompt ::
  -- | 'prompt'
  Prompt ->
  -- | 'rejectionStatement'
  Statement ->
  FollowUpPrompt
newFollowUpPrompt pPrompt_ pRejectionStatement_ =
  FollowUpPrompt'
    { prompt = pPrompt_,
      rejectionStatement = pRejectionStatement_
    }

-- | Prompts for information from the user.
followUpPrompt_prompt :: Lens.Lens' FollowUpPrompt Prompt
followUpPrompt_prompt = Lens.lens (\FollowUpPrompt' {prompt} -> prompt) (\s@FollowUpPrompt' {} a -> s {prompt = a} :: FollowUpPrompt)

-- | If the user answers \"no\" to the question defined in the @prompt@
-- field, Amazon Lex responds with this statement to acknowledge that the
-- intent was canceled.
followUpPrompt_rejectionStatement :: Lens.Lens' FollowUpPrompt Statement
followUpPrompt_rejectionStatement = Lens.lens (\FollowUpPrompt' {rejectionStatement} -> rejectionStatement) (\s@FollowUpPrompt' {} a -> s {rejectionStatement = a} :: FollowUpPrompt)

instance Data.FromJSON FollowUpPrompt where
  parseJSON =
    Data.withObject
      "FollowUpPrompt"
      ( \x ->
          FollowUpPrompt'
            Prelude.<$> (x Data..: "prompt")
            Prelude.<*> (x Data..: "rejectionStatement")
      )

instance Prelude.Hashable FollowUpPrompt where
  hashWithSalt _salt FollowUpPrompt' {..} =
    _salt
      `Prelude.hashWithSalt` prompt
      `Prelude.hashWithSalt` rejectionStatement

instance Prelude.NFData FollowUpPrompt where
  rnf FollowUpPrompt' {..} =
    Prelude.rnf prompt `Prelude.seq`
      Prelude.rnf rejectionStatement

instance Data.ToJSON FollowUpPrompt where
  toJSON FollowUpPrompt' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("prompt" Data..= prompt),
            Prelude.Just
              ("rejectionStatement" Data..= rejectionStatement)
          ]
      )
