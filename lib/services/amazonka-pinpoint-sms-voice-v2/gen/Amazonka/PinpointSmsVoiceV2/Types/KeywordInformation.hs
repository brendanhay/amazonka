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
-- Module      : Amazonka.PinpointSmsVoiceV2.Types.KeywordInformation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PinpointSmsVoiceV2.Types.KeywordInformation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.PinpointSmsVoiceV2.Types.KeywordAction
import qualified Amazonka.Prelude as Prelude

-- | The information for all keywords in a pool.
--
-- /See:/ 'newKeywordInformation' smart constructor.
data KeywordInformation = KeywordInformation'
  { -- | The keyword as a string.
    keyword :: Prelude.Text,
    -- | A custom message that can be used with the keyword.
    keywordMessage :: Prelude.Text,
    -- | The action to perform for the keyword.
    keywordAction :: KeywordAction
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'KeywordInformation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'keyword', 'keywordInformation_keyword' - The keyword as a string.
--
-- 'keywordMessage', 'keywordInformation_keywordMessage' - A custom message that can be used with the keyword.
--
-- 'keywordAction', 'keywordInformation_keywordAction' - The action to perform for the keyword.
newKeywordInformation ::
  -- | 'keyword'
  Prelude.Text ->
  -- | 'keywordMessage'
  Prelude.Text ->
  -- | 'keywordAction'
  KeywordAction ->
  KeywordInformation
newKeywordInformation
  pKeyword_
  pKeywordMessage_
  pKeywordAction_ =
    KeywordInformation'
      { keyword = pKeyword_,
        keywordMessage = pKeywordMessage_,
        keywordAction = pKeywordAction_
      }

-- | The keyword as a string.
keywordInformation_keyword :: Lens.Lens' KeywordInformation Prelude.Text
keywordInformation_keyword = Lens.lens (\KeywordInformation' {keyword} -> keyword) (\s@KeywordInformation' {} a -> s {keyword = a} :: KeywordInformation)

-- | A custom message that can be used with the keyword.
keywordInformation_keywordMessage :: Lens.Lens' KeywordInformation Prelude.Text
keywordInformation_keywordMessage = Lens.lens (\KeywordInformation' {keywordMessage} -> keywordMessage) (\s@KeywordInformation' {} a -> s {keywordMessage = a} :: KeywordInformation)

-- | The action to perform for the keyword.
keywordInformation_keywordAction :: Lens.Lens' KeywordInformation KeywordAction
keywordInformation_keywordAction = Lens.lens (\KeywordInformation' {keywordAction} -> keywordAction) (\s@KeywordInformation' {} a -> s {keywordAction = a} :: KeywordInformation)

instance Data.FromJSON KeywordInformation where
  parseJSON =
    Data.withObject
      "KeywordInformation"
      ( \x ->
          KeywordInformation'
            Prelude.<$> (x Data..: "Keyword")
            Prelude.<*> (x Data..: "KeywordMessage")
            Prelude.<*> (x Data..: "KeywordAction")
      )

instance Prelude.Hashable KeywordInformation where
  hashWithSalt _salt KeywordInformation' {..} =
    _salt `Prelude.hashWithSalt` keyword
      `Prelude.hashWithSalt` keywordMessage
      `Prelude.hashWithSalt` keywordAction

instance Prelude.NFData KeywordInformation where
  rnf KeywordInformation' {..} =
    Prelude.rnf keyword
      `Prelude.seq` Prelude.rnf keywordMessage
      `Prelude.seq` Prelude.rnf keywordAction
