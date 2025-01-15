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
-- Module      : Amazonka.LexModels.Types.UtteranceList
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexModels.Types.UtteranceList where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexModels.Types.UtteranceData
import qualified Amazonka.Prelude as Prelude

-- | Provides a list of utterances that have been made to a specific version
-- of your bot. The list contains a maximum of 100 utterances.
--
-- /See:/ 'newUtteranceList' smart constructor.
data UtteranceList = UtteranceList'
  { -- | The version of the bot that processed the list.
    botVersion :: Prelude.Maybe Prelude.Text,
    -- | One or more UtteranceData objects that contain information about the
    -- utterances that have been made to a bot. The maximum number of object is
    -- 100.
    utterances :: Prelude.Maybe [UtteranceData]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UtteranceList' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'botVersion', 'utteranceList_botVersion' - The version of the bot that processed the list.
--
-- 'utterances', 'utteranceList_utterances' - One or more UtteranceData objects that contain information about the
-- utterances that have been made to a bot. The maximum number of object is
-- 100.
newUtteranceList ::
  UtteranceList
newUtteranceList =
  UtteranceList'
    { botVersion = Prelude.Nothing,
      utterances = Prelude.Nothing
    }

-- | The version of the bot that processed the list.
utteranceList_botVersion :: Lens.Lens' UtteranceList (Prelude.Maybe Prelude.Text)
utteranceList_botVersion = Lens.lens (\UtteranceList' {botVersion} -> botVersion) (\s@UtteranceList' {} a -> s {botVersion = a} :: UtteranceList)

-- | One or more UtteranceData objects that contain information about the
-- utterances that have been made to a bot. The maximum number of object is
-- 100.
utteranceList_utterances :: Lens.Lens' UtteranceList (Prelude.Maybe [UtteranceData])
utteranceList_utterances = Lens.lens (\UtteranceList' {utterances} -> utterances) (\s@UtteranceList' {} a -> s {utterances = a} :: UtteranceList) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON UtteranceList where
  parseJSON =
    Data.withObject
      "UtteranceList"
      ( \x ->
          UtteranceList'
            Prelude.<$> (x Data..:? "botVersion")
            Prelude.<*> (x Data..:? "utterances" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable UtteranceList where
  hashWithSalt _salt UtteranceList' {..} =
    _salt
      `Prelude.hashWithSalt` botVersion
      `Prelude.hashWithSalt` utterances

instance Prelude.NFData UtteranceList where
  rnf UtteranceList' {..} =
    Prelude.rnf botVersion `Prelude.seq`
      Prelude.rnf utterances
