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
-- Module      : Amazonka.MediaConvert.Types.Xavc4kIntraVbrProfileSettings
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.Xavc4kIntraVbrProfileSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MediaConvert.Types.Xavc4kIntraVbrProfileClass
import qualified Amazonka.Prelude as Prelude

-- | Required when you set (Profile) under
-- (VideoDescription)>(CodecSettings)>(XavcSettings) to the value
-- XAVC_4K_INTRA_VBR.
--
-- /See:/ 'newXavc4kIntraVbrProfileSettings' smart constructor.
data Xavc4kIntraVbrProfileSettings = Xavc4kIntraVbrProfileSettings'
  { -- | Specify the XAVC Intra 4k (VBR) Class to set the bitrate of your output.
    -- Outputs of the same class have similar image quality over the operating
    -- points that are valid for that class.
    xavcClass :: Prelude.Maybe Xavc4kIntraVbrProfileClass
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Xavc4kIntraVbrProfileSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'xavcClass', 'xavc4kIntraVbrProfileSettings_xavcClass' - Specify the XAVC Intra 4k (VBR) Class to set the bitrate of your output.
-- Outputs of the same class have similar image quality over the operating
-- points that are valid for that class.
newXavc4kIntraVbrProfileSettings ::
  Xavc4kIntraVbrProfileSettings
newXavc4kIntraVbrProfileSettings =
  Xavc4kIntraVbrProfileSettings'
    { xavcClass =
        Prelude.Nothing
    }

-- | Specify the XAVC Intra 4k (VBR) Class to set the bitrate of your output.
-- Outputs of the same class have similar image quality over the operating
-- points that are valid for that class.
xavc4kIntraVbrProfileSettings_xavcClass :: Lens.Lens' Xavc4kIntraVbrProfileSettings (Prelude.Maybe Xavc4kIntraVbrProfileClass)
xavc4kIntraVbrProfileSettings_xavcClass = Lens.lens (\Xavc4kIntraVbrProfileSettings' {xavcClass} -> xavcClass) (\s@Xavc4kIntraVbrProfileSettings' {} a -> s {xavcClass = a} :: Xavc4kIntraVbrProfileSettings)

instance Core.FromJSON Xavc4kIntraVbrProfileSettings where
  parseJSON =
    Core.withObject
      "Xavc4kIntraVbrProfileSettings"
      ( \x ->
          Xavc4kIntraVbrProfileSettings'
            Prelude.<$> (x Core..:? "xavcClass")
      )

instance
  Prelude.Hashable
    Xavc4kIntraVbrProfileSettings
  where
  hashWithSalt _salt Xavc4kIntraVbrProfileSettings' {..} =
    _salt `Prelude.hashWithSalt` xavcClass

instance Prelude.NFData Xavc4kIntraVbrProfileSettings where
  rnf Xavc4kIntraVbrProfileSettings' {..} =
    Prelude.rnf xavcClass

instance Core.ToJSON Xavc4kIntraVbrProfileSettings where
  toJSON Xavc4kIntraVbrProfileSettings' {..} =
    Core.object
      ( Prelude.catMaybes
          [("xavcClass" Core..=) Prelude.<$> xavcClass]
      )
