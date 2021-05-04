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
-- Module      : Network.AWS.LexModels.Types.FollowUpPrompt
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexModels.Types.FollowUpPrompt where

import qualified Network.AWS.Lens as Lens
import Network.AWS.LexModels.Types.Prompt
import Network.AWS.LexModels.Types.Statement
import qualified Network.AWS.Prelude as Prelude

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.FromJSON FollowUpPrompt where
  parseJSON =
    Prelude.withObject
      "FollowUpPrompt"
      ( \x ->
          FollowUpPrompt'
            Prelude.<$> (x Prelude..: "prompt")
            Prelude.<*> (x Prelude..: "rejectionStatement")
      )

instance Prelude.Hashable FollowUpPrompt

instance Prelude.NFData FollowUpPrompt

instance Prelude.ToJSON FollowUpPrompt where
  toJSON FollowUpPrompt' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("prompt" Prelude..= prompt),
            Prelude.Just
              ( "rejectionStatement"
                  Prelude..= rejectionStatement
              )
          ]
      )
