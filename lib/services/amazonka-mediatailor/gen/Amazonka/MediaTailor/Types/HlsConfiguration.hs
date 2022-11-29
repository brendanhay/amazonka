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
-- Module      : Amazonka.MediaTailor.Types.HlsConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaTailor.Types.HlsConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The configuration for HLS content.
--
-- /See:/ 'newHlsConfiguration' smart constructor.
data HlsConfiguration = HlsConfiguration'
  { -- | The URL that is used to initiate a playback session for devices that
    -- support Apple HLS. The session uses server-side reporting.
    manifestEndpointPrefix :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HlsConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'manifestEndpointPrefix', 'hlsConfiguration_manifestEndpointPrefix' - The URL that is used to initiate a playback session for devices that
-- support Apple HLS. The session uses server-side reporting.
newHlsConfiguration ::
  HlsConfiguration
newHlsConfiguration =
  HlsConfiguration'
    { manifestEndpointPrefix =
        Prelude.Nothing
    }

-- | The URL that is used to initiate a playback session for devices that
-- support Apple HLS. The session uses server-side reporting.
hlsConfiguration_manifestEndpointPrefix :: Lens.Lens' HlsConfiguration (Prelude.Maybe Prelude.Text)
hlsConfiguration_manifestEndpointPrefix = Lens.lens (\HlsConfiguration' {manifestEndpointPrefix} -> manifestEndpointPrefix) (\s@HlsConfiguration' {} a -> s {manifestEndpointPrefix = a} :: HlsConfiguration)

instance Core.FromJSON HlsConfiguration where
  parseJSON =
    Core.withObject
      "HlsConfiguration"
      ( \x ->
          HlsConfiguration'
            Prelude.<$> (x Core..:? "ManifestEndpointPrefix")
      )

instance Prelude.Hashable HlsConfiguration where
  hashWithSalt _salt HlsConfiguration' {..} =
    _salt `Prelude.hashWithSalt` manifestEndpointPrefix

instance Prelude.NFData HlsConfiguration where
  rnf HlsConfiguration' {..} =
    Prelude.rnf manifestEndpointPrefix
