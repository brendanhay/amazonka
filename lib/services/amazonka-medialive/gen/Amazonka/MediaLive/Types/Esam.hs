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
-- Module      : Amazonka.MediaLive.Types.Esam
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.Esam where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Esam
--
-- /See:/ 'newEsam' smart constructor.
data Esam = Esam'
  { -- | When specified, this offset (in milliseconds) is added to the input Ad
    -- Avail PTS time. This only applies to embedded SCTE 104\/35 messages and
    -- does not apply to OOB messages.
    adAvailOffset :: Prelude.Maybe Prelude.Int,
    -- | Documentation update needed
    passwordParam :: Prelude.Maybe Prelude.Text,
    -- | Documentation update needed
    username :: Prelude.Maybe Prelude.Text,
    -- | Optional data sent as zoneIdentity to identify the MediaLive channel to
    -- the POIS.
    zoneIdentity :: Prelude.Maybe Prelude.Text,
    -- | Sent as acquisitionPointIdentity to identify the MediaLive channel to
    -- the POIS.
    acquisitionPointId :: Prelude.Text,
    -- | The URL of the signal conditioner endpoint on the Placement Opportunity
    -- Information System (POIS). MediaLive sends SignalProcessingEvents here
    -- when SCTE-35 messages are read.
    poisEndpoint :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Esam' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'adAvailOffset', 'esam_adAvailOffset' - When specified, this offset (in milliseconds) is added to the input Ad
-- Avail PTS time. This only applies to embedded SCTE 104\/35 messages and
-- does not apply to OOB messages.
--
-- 'passwordParam', 'esam_passwordParam' - Documentation update needed
--
-- 'username', 'esam_username' - Documentation update needed
--
-- 'zoneIdentity', 'esam_zoneIdentity' - Optional data sent as zoneIdentity to identify the MediaLive channel to
-- the POIS.
--
-- 'acquisitionPointId', 'esam_acquisitionPointId' - Sent as acquisitionPointIdentity to identify the MediaLive channel to
-- the POIS.
--
-- 'poisEndpoint', 'esam_poisEndpoint' - The URL of the signal conditioner endpoint on the Placement Opportunity
-- Information System (POIS). MediaLive sends SignalProcessingEvents here
-- when SCTE-35 messages are read.
newEsam ::
  -- | 'acquisitionPointId'
  Prelude.Text ->
  -- | 'poisEndpoint'
  Prelude.Text ->
  Esam
newEsam pAcquisitionPointId_ pPoisEndpoint_ =
  Esam'
    { adAvailOffset = Prelude.Nothing,
      passwordParam = Prelude.Nothing,
      username = Prelude.Nothing,
      zoneIdentity = Prelude.Nothing,
      acquisitionPointId = pAcquisitionPointId_,
      poisEndpoint = pPoisEndpoint_
    }

-- | When specified, this offset (in milliseconds) is added to the input Ad
-- Avail PTS time. This only applies to embedded SCTE 104\/35 messages and
-- does not apply to OOB messages.
esam_adAvailOffset :: Lens.Lens' Esam (Prelude.Maybe Prelude.Int)
esam_adAvailOffset = Lens.lens (\Esam' {adAvailOffset} -> adAvailOffset) (\s@Esam' {} a -> s {adAvailOffset = a} :: Esam)

-- | Documentation update needed
esam_passwordParam :: Lens.Lens' Esam (Prelude.Maybe Prelude.Text)
esam_passwordParam = Lens.lens (\Esam' {passwordParam} -> passwordParam) (\s@Esam' {} a -> s {passwordParam = a} :: Esam)

-- | Documentation update needed
esam_username :: Lens.Lens' Esam (Prelude.Maybe Prelude.Text)
esam_username = Lens.lens (\Esam' {username} -> username) (\s@Esam' {} a -> s {username = a} :: Esam)

-- | Optional data sent as zoneIdentity to identify the MediaLive channel to
-- the POIS.
esam_zoneIdentity :: Lens.Lens' Esam (Prelude.Maybe Prelude.Text)
esam_zoneIdentity = Lens.lens (\Esam' {zoneIdentity} -> zoneIdentity) (\s@Esam' {} a -> s {zoneIdentity = a} :: Esam)

-- | Sent as acquisitionPointIdentity to identify the MediaLive channel to
-- the POIS.
esam_acquisitionPointId :: Lens.Lens' Esam Prelude.Text
esam_acquisitionPointId = Lens.lens (\Esam' {acquisitionPointId} -> acquisitionPointId) (\s@Esam' {} a -> s {acquisitionPointId = a} :: Esam)

-- | The URL of the signal conditioner endpoint on the Placement Opportunity
-- Information System (POIS). MediaLive sends SignalProcessingEvents here
-- when SCTE-35 messages are read.
esam_poisEndpoint :: Lens.Lens' Esam Prelude.Text
esam_poisEndpoint = Lens.lens (\Esam' {poisEndpoint} -> poisEndpoint) (\s@Esam' {} a -> s {poisEndpoint = a} :: Esam)

instance Data.FromJSON Esam where
  parseJSON =
    Data.withObject
      "Esam"
      ( \x ->
          Esam'
            Prelude.<$> (x Data..:? "adAvailOffset")
            Prelude.<*> (x Data..:? "passwordParam")
            Prelude.<*> (x Data..:? "username")
            Prelude.<*> (x Data..:? "zoneIdentity")
            Prelude.<*> (x Data..: "acquisitionPointId")
            Prelude.<*> (x Data..: "poisEndpoint")
      )

instance Prelude.Hashable Esam where
  hashWithSalt _salt Esam' {..} =
    _salt
      `Prelude.hashWithSalt` adAvailOffset
      `Prelude.hashWithSalt` passwordParam
      `Prelude.hashWithSalt` username
      `Prelude.hashWithSalt` zoneIdentity
      `Prelude.hashWithSalt` acquisitionPointId
      `Prelude.hashWithSalt` poisEndpoint

instance Prelude.NFData Esam where
  rnf Esam' {..} =
    Prelude.rnf adAvailOffset
      `Prelude.seq` Prelude.rnf passwordParam
      `Prelude.seq` Prelude.rnf username
      `Prelude.seq` Prelude.rnf zoneIdentity
      `Prelude.seq` Prelude.rnf acquisitionPointId
      `Prelude.seq` Prelude.rnf poisEndpoint

instance Data.ToJSON Esam where
  toJSON Esam' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("adAvailOffset" Data..=) Prelude.<$> adAvailOffset,
            ("passwordParam" Data..=) Prelude.<$> passwordParam,
            ("username" Data..=) Prelude.<$> username,
            ("zoneIdentity" Data..=) Prelude.<$> zoneIdentity,
            Prelude.Just
              ("acquisitionPointId" Data..= acquisitionPointId),
            Prelude.Just ("poisEndpoint" Data..= poisEndpoint)
          ]
      )
