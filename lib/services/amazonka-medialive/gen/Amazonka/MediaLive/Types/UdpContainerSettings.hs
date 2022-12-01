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
-- Module      : Amazonka.MediaLive.Types.UdpContainerSettings
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.UdpContainerSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MediaLive.Types.M2tsSettings
import qualified Amazonka.Prelude as Prelude

-- | Udp Container Settings
--
-- /See:/ 'newUdpContainerSettings' smart constructor.
data UdpContainerSettings = UdpContainerSettings'
  { m2tsSettings :: Prelude.Maybe M2tsSettings
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UdpContainerSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'm2tsSettings', 'udpContainerSettings_m2tsSettings' - Undocumented member.
newUdpContainerSettings ::
  UdpContainerSettings
newUdpContainerSettings =
  UdpContainerSettings'
    { m2tsSettings =
        Prelude.Nothing
    }

-- | Undocumented member.
udpContainerSettings_m2tsSettings :: Lens.Lens' UdpContainerSettings (Prelude.Maybe M2tsSettings)
udpContainerSettings_m2tsSettings = Lens.lens (\UdpContainerSettings' {m2tsSettings} -> m2tsSettings) (\s@UdpContainerSettings' {} a -> s {m2tsSettings = a} :: UdpContainerSettings)

instance Core.FromJSON UdpContainerSettings where
  parseJSON =
    Core.withObject
      "UdpContainerSettings"
      ( \x ->
          UdpContainerSettings'
            Prelude.<$> (x Core..:? "m2tsSettings")
      )

instance Prelude.Hashable UdpContainerSettings where
  hashWithSalt _salt UdpContainerSettings' {..} =
    _salt `Prelude.hashWithSalt` m2tsSettings

instance Prelude.NFData UdpContainerSettings where
  rnf UdpContainerSettings' {..} =
    Prelude.rnf m2tsSettings

instance Core.ToJSON UdpContainerSettings where
  toJSON UdpContainerSettings' {..} =
    Core.object
      ( Prelude.catMaybes
          [("m2tsSettings" Core..=) Prelude.<$> m2tsSettings]
      )
