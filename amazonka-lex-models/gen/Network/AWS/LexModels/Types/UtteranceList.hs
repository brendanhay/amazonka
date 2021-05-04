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
-- Module      : Network.AWS.LexModels.Types.UtteranceList
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexModels.Types.UtteranceList where

import qualified Network.AWS.Lens as Lens
import Network.AWS.LexModels.Types.UtteranceData
import qualified Network.AWS.Prelude as Prelude

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
utteranceList_utterances = Lens.lens (\UtteranceList' {utterances} -> utterances) (\s@UtteranceList' {} a -> s {utterances = a} :: UtteranceList) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromJSON UtteranceList where
  parseJSON =
    Prelude.withObject
      "UtteranceList"
      ( \x ->
          UtteranceList'
            Prelude.<$> (x Prelude..:? "botVersion")
            Prelude.<*> ( x Prelude..:? "utterances"
                            Prelude..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable UtteranceList

instance Prelude.NFData UtteranceList
