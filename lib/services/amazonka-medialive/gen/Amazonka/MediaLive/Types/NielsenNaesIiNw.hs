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
-- Module      : Amazonka.MediaLive.Types.NielsenNaesIiNw
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.NielsenNaesIiNw where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaLive.Types.NielsenWatermarkTimezones
import qualified Amazonka.Prelude as Prelude

-- | Nielsen Naes Ii Nw
--
-- /See:/ 'newNielsenNaesIiNw' smart constructor.
data NielsenNaesIiNw = NielsenNaesIiNw'
  { -- | Choose the timezone for the time stamps in the watermark. If not
    -- provided, the timestamps will be in Coordinated Universal Time (UTC)
    timezone :: Prelude.Maybe NielsenWatermarkTimezones,
    -- | Enter the check digit string for the watermark
    checkDigitString :: Prelude.Text,
    -- | Enter the Nielsen Source ID (SID) to include in the watermark
    sid :: Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NielsenNaesIiNw' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'timezone', 'nielsenNaesIiNw_timezone' - Choose the timezone for the time stamps in the watermark. If not
-- provided, the timestamps will be in Coordinated Universal Time (UTC)
--
-- 'checkDigitString', 'nielsenNaesIiNw_checkDigitString' - Enter the check digit string for the watermark
--
-- 'sid', 'nielsenNaesIiNw_sid' - Enter the Nielsen Source ID (SID) to include in the watermark
newNielsenNaesIiNw ::
  -- | 'checkDigitString'
  Prelude.Text ->
  -- | 'sid'
  Prelude.Double ->
  NielsenNaesIiNw
newNielsenNaesIiNw pCheckDigitString_ pSid_ =
  NielsenNaesIiNw'
    { timezone = Prelude.Nothing,
      checkDigitString = pCheckDigitString_,
      sid = pSid_
    }

-- | Choose the timezone for the time stamps in the watermark. If not
-- provided, the timestamps will be in Coordinated Universal Time (UTC)
nielsenNaesIiNw_timezone :: Lens.Lens' NielsenNaesIiNw (Prelude.Maybe NielsenWatermarkTimezones)
nielsenNaesIiNw_timezone = Lens.lens (\NielsenNaesIiNw' {timezone} -> timezone) (\s@NielsenNaesIiNw' {} a -> s {timezone = a} :: NielsenNaesIiNw)

-- | Enter the check digit string for the watermark
nielsenNaesIiNw_checkDigitString :: Lens.Lens' NielsenNaesIiNw Prelude.Text
nielsenNaesIiNw_checkDigitString = Lens.lens (\NielsenNaesIiNw' {checkDigitString} -> checkDigitString) (\s@NielsenNaesIiNw' {} a -> s {checkDigitString = a} :: NielsenNaesIiNw)

-- | Enter the Nielsen Source ID (SID) to include in the watermark
nielsenNaesIiNw_sid :: Lens.Lens' NielsenNaesIiNw Prelude.Double
nielsenNaesIiNw_sid = Lens.lens (\NielsenNaesIiNw' {sid} -> sid) (\s@NielsenNaesIiNw' {} a -> s {sid = a} :: NielsenNaesIiNw)

instance Data.FromJSON NielsenNaesIiNw where
  parseJSON =
    Data.withObject
      "NielsenNaesIiNw"
      ( \x ->
          NielsenNaesIiNw'
            Prelude.<$> (x Data..:? "timezone")
            Prelude.<*> (x Data..: "checkDigitString")
            Prelude.<*> (x Data..: "sid")
      )

instance Prelude.Hashable NielsenNaesIiNw where
  hashWithSalt _salt NielsenNaesIiNw' {..} =
    _salt
      `Prelude.hashWithSalt` timezone
      `Prelude.hashWithSalt` checkDigitString
      `Prelude.hashWithSalt` sid

instance Prelude.NFData NielsenNaesIiNw where
  rnf NielsenNaesIiNw' {..} =
    Prelude.rnf timezone
      `Prelude.seq` Prelude.rnf checkDigitString
      `Prelude.seq` Prelude.rnf sid

instance Data.ToJSON NielsenNaesIiNw where
  toJSON NielsenNaesIiNw' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("timezone" Data..=) Prelude.<$> timezone,
            Prelude.Just
              ("checkDigitString" Data..= checkDigitString),
            Prelude.Just ("sid" Data..= sid)
          ]
      )
