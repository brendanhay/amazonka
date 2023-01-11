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
-- Module      : Amazonka.MediaConnect.Types.MediaStreamAttributes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConnect.Types.MediaStreamAttributes where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConnect.Types.Fmtp
import qualified Amazonka.Prelude as Prelude

-- | Attributes that are related to the media stream.
--
-- /See:/ 'newMediaStreamAttributes' smart constructor.
data MediaStreamAttributes = MediaStreamAttributes'
  { -- | The audio language, in a format that is recognized by the receiver.
    lang :: Prelude.Maybe Prelude.Text,
    -- | A set of parameters that define the media stream.
    fmtp :: Fmtp
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MediaStreamAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lang', 'mediaStreamAttributes_lang' - The audio language, in a format that is recognized by the receiver.
--
-- 'fmtp', 'mediaStreamAttributes_fmtp' - A set of parameters that define the media stream.
newMediaStreamAttributes ::
  -- | 'fmtp'
  Fmtp ->
  MediaStreamAttributes
newMediaStreamAttributes pFmtp_ =
  MediaStreamAttributes'
    { lang = Prelude.Nothing,
      fmtp = pFmtp_
    }

-- | The audio language, in a format that is recognized by the receiver.
mediaStreamAttributes_lang :: Lens.Lens' MediaStreamAttributes (Prelude.Maybe Prelude.Text)
mediaStreamAttributes_lang = Lens.lens (\MediaStreamAttributes' {lang} -> lang) (\s@MediaStreamAttributes' {} a -> s {lang = a} :: MediaStreamAttributes)

-- | A set of parameters that define the media stream.
mediaStreamAttributes_fmtp :: Lens.Lens' MediaStreamAttributes Fmtp
mediaStreamAttributes_fmtp = Lens.lens (\MediaStreamAttributes' {fmtp} -> fmtp) (\s@MediaStreamAttributes' {} a -> s {fmtp = a} :: MediaStreamAttributes)

instance Data.FromJSON MediaStreamAttributes where
  parseJSON =
    Data.withObject
      "MediaStreamAttributes"
      ( \x ->
          MediaStreamAttributes'
            Prelude.<$> (x Data..:? "lang") Prelude.<*> (x Data..: "fmtp")
      )

instance Prelude.Hashable MediaStreamAttributes where
  hashWithSalt _salt MediaStreamAttributes' {..} =
    _salt `Prelude.hashWithSalt` lang
      `Prelude.hashWithSalt` fmtp

instance Prelude.NFData MediaStreamAttributes where
  rnf MediaStreamAttributes' {..} =
    Prelude.rnf lang `Prelude.seq` Prelude.rnf fmtp
