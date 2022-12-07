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
-- Module      : Amazonka.MediaConvert.Types.OutputSettings
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.OutputSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConvert.Types.HlsSettings
import qualified Amazonka.Prelude as Prelude

-- | Specific settings for this type of output.
--
-- /See:/ 'newOutputSettings' smart constructor.
data OutputSettings = OutputSettings'
  { -- | Settings for HLS output groups
    hlsSettings :: Prelude.Maybe HlsSettings
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OutputSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hlsSettings', 'outputSettings_hlsSettings' - Settings for HLS output groups
newOutputSettings ::
  OutputSettings
newOutputSettings =
  OutputSettings' {hlsSettings = Prelude.Nothing}

-- | Settings for HLS output groups
outputSettings_hlsSettings :: Lens.Lens' OutputSettings (Prelude.Maybe HlsSettings)
outputSettings_hlsSettings = Lens.lens (\OutputSettings' {hlsSettings} -> hlsSettings) (\s@OutputSettings' {} a -> s {hlsSettings = a} :: OutputSettings)

instance Data.FromJSON OutputSettings where
  parseJSON =
    Data.withObject
      "OutputSettings"
      ( \x ->
          OutputSettings'
            Prelude.<$> (x Data..:? "hlsSettings")
      )

instance Prelude.Hashable OutputSettings where
  hashWithSalt _salt OutputSettings' {..} =
    _salt `Prelude.hashWithSalt` hlsSettings

instance Prelude.NFData OutputSettings where
  rnf OutputSettings' {..} = Prelude.rnf hlsSettings

instance Data.ToJSON OutputSettings where
  toJSON OutputSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [("hlsSettings" Data..=) Prelude.<$> hlsSettings]
      )
