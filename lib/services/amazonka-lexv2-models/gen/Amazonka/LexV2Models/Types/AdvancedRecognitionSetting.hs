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
-- Module      : Amazonka.LexV2Models.Types.AdvancedRecognitionSetting
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.AdvancedRecognitionSetting where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.LexV2Models.Types.AudioRecognitionStrategy
import qualified Amazonka.Prelude as Prelude

-- | Provides settings that enable advanced recognition settings for slot
-- values.
--
-- /See:/ 'newAdvancedRecognitionSetting' smart constructor.
data AdvancedRecognitionSetting = AdvancedRecognitionSetting'
  { -- | Enables using the slot values as a custom vocabulary for recognizing
    -- user utterances.
    audioRecognitionStrategy :: Prelude.Maybe AudioRecognitionStrategy
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AdvancedRecognitionSetting' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'audioRecognitionStrategy', 'advancedRecognitionSetting_audioRecognitionStrategy' - Enables using the slot values as a custom vocabulary for recognizing
-- user utterances.
newAdvancedRecognitionSetting ::
  AdvancedRecognitionSetting
newAdvancedRecognitionSetting =
  AdvancedRecognitionSetting'
    { audioRecognitionStrategy =
        Prelude.Nothing
    }

-- | Enables using the slot values as a custom vocabulary for recognizing
-- user utterances.
advancedRecognitionSetting_audioRecognitionStrategy :: Lens.Lens' AdvancedRecognitionSetting (Prelude.Maybe AudioRecognitionStrategy)
advancedRecognitionSetting_audioRecognitionStrategy = Lens.lens (\AdvancedRecognitionSetting' {audioRecognitionStrategy} -> audioRecognitionStrategy) (\s@AdvancedRecognitionSetting' {} a -> s {audioRecognitionStrategy = a} :: AdvancedRecognitionSetting)

instance Core.FromJSON AdvancedRecognitionSetting where
  parseJSON =
    Core.withObject
      "AdvancedRecognitionSetting"
      ( \x ->
          AdvancedRecognitionSetting'
            Prelude.<$> (x Core..:? "audioRecognitionStrategy")
      )

instance Prelude.Hashable AdvancedRecognitionSetting where
  hashWithSalt _salt AdvancedRecognitionSetting' {..} =
    _salt
      `Prelude.hashWithSalt` audioRecognitionStrategy

instance Prelude.NFData AdvancedRecognitionSetting where
  rnf AdvancedRecognitionSetting' {..} =
    Prelude.rnf audioRecognitionStrategy

instance Core.ToJSON AdvancedRecognitionSetting where
  toJSON AdvancedRecognitionSetting' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("audioRecognitionStrategy" Core..=)
              Prelude.<$> audioRecognitionStrategy
          ]
      )
