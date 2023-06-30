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
-- Module      : Amazonka.MediaConvert.Types.OutputGroupSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.OutputGroupSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConvert.Types.CmafGroupSettings
import Amazonka.MediaConvert.Types.DashIsoGroupSettings
import Amazonka.MediaConvert.Types.FileGroupSettings
import Amazonka.MediaConvert.Types.HlsGroupSettings
import Amazonka.MediaConvert.Types.MsSmoothGroupSettings
import Amazonka.MediaConvert.Types.OutputGroupType
import qualified Amazonka.Prelude as Prelude

-- | Output Group settings, including type
--
-- /See:/ 'newOutputGroupSettings' smart constructor.
data OutputGroupSettings = OutputGroupSettings'
  { -- | Settings related to your CMAF output package. For more information, see
    -- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/outputs-file-ABR.html.
    -- When you work directly in your JSON job specification, include this
    -- object and any required children when you set Type, under
    -- OutputGroupSettings, to CMAF_GROUP_SETTINGS.
    cmafGroupSettings :: Prelude.Maybe CmafGroupSettings,
    -- | Settings related to your DASH output package. For more information, see
    -- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/outputs-file-ABR.html.
    -- When you work directly in your JSON job specification, include this
    -- object and any required children when you set Type, under
    -- OutputGroupSettings, to DASH_ISO_GROUP_SETTINGS.
    dashIsoGroupSettings :: Prelude.Maybe DashIsoGroupSettings,
    -- | Settings related to your File output group. MediaConvert uses this group
    -- of settings to generate a single standalone file, rather than a
    -- streaming package. When you work directly in your JSON job
    -- specification, include this object and any required children when you
    -- set Type, under OutputGroupSettings, to FILE_GROUP_SETTINGS.
    fileGroupSettings :: Prelude.Maybe FileGroupSettings,
    -- | Settings related to your HLS output package. For more information, see
    -- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/outputs-file-ABR.html.
    -- When you work directly in your JSON job specification, include this
    -- object and any required children when you set Type, under
    -- OutputGroupSettings, to HLS_GROUP_SETTINGS.
    hlsGroupSettings :: Prelude.Maybe HlsGroupSettings,
    -- | Settings related to your Microsoft Smooth Streaming output package. For
    -- more information, see
    -- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/outputs-file-ABR.html.
    -- When you work directly in your JSON job specification, include this
    -- object and any required children when you set Type, under
    -- OutputGroupSettings, to MS_SMOOTH_GROUP_SETTINGS.
    msSmoothGroupSettings :: Prelude.Maybe MsSmoothGroupSettings,
    -- | Type of output group (File group, Apple HLS, DASH ISO, Microsoft Smooth
    -- Streaming, CMAF)
    type' :: Prelude.Maybe OutputGroupType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OutputGroupSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cmafGroupSettings', 'outputGroupSettings_cmafGroupSettings' - Settings related to your CMAF output package. For more information, see
-- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/outputs-file-ABR.html.
-- When you work directly in your JSON job specification, include this
-- object and any required children when you set Type, under
-- OutputGroupSettings, to CMAF_GROUP_SETTINGS.
--
-- 'dashIsoGroupSettings', 'outputGroupSettings_dashIsoGroupSettings' - Settings related to your DASH output package. For more information, see
-- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/outputs-file-ABR.html.
-- When you work directly in your JSON job specification, include this
-- object and any required children when you set Type, under
-- OutputGroupSettings, to DASH_ISO_GROUP_SETTINGS.
--
-- 'fileGroupSettings', 'outputGroupSettings_fileGroupSettings' - Settings related to your File output group. MediaConvert uses this group
-- of settings to generate a single standalone file, rather than a
-- streaming package. When you work directly in your JSON job
-- specification, include this object and any required children when you
-- set Type, under OutputGroupSettings, to FILE_GROUP_SETTINGS.
--
-- 'hlsGroupSettings', 'outputGroupSettings_hlsGroupSettings' - Settings related to your HLS output package. For more information, see
-- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/outputs-file-ABR.html.
-- When you work directly in your JSON job specification, include this
-- object and any required children when you set Type, under
-- OutputGroupSettings, to HLS_GROUP_SETTINGS.
--
-- 'msSmoothGroupSettings', 'outputGroupSettings_msSmoothGroupSettings' - Settings related to your Microsoft Smooth Streaming output package. For
-- more information, see
-- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/outputs-file-ABR.html.
-- When you work directly in your JSON job specification, include this
-- object and any required children when you set Type, under
-- OutputGroupSettings, to MS_SMOOTH_GROUP_SETTINGS.
--
-- 'type'', 'outputGroupSettings_type' - Type of output group (File group, Apple HLS, DASH ISO, Microsoft Smooth
-- Streaming, CMAF)
newOutputGroupSettings ::
  OutputGroupSettings
newOutputGroupSettings =
  OutputGroupSettings'
    { cmafGroupSettings =
        Prelude.Nothing,
      dashIsoGroupSettings = Prelude.Nothing,
      fileGroupSettings = Prelude.Nothing,
      hlsGroupSettings = Prelude.Nothing,
      msSmoothGroupSettings = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | Settings related to your CMAF output package. For more information, see
-- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/outputs-file-ABR.html.
-- When you work directly in your JSON job specification, include this
-- object and any required children when you set Type, under
-- OutputGroupSettings, to CMAF_GROUP_SETTINGS.
outputGroupSettings_cmafGroupSettings :: Lens.Lens' OutputGroupSettings (Prelude.Maybe CmafGroupSettings)
outputGroupSettings_cmafGroupSettings = Lens.lens (\OutputGroupSettings' {cmafGroupSettings} -> cmafGroupSettings) (\s@OutputGroupSettings' {} a -> s {cmafGroupSettings = a} :: OutputGroupSettings)

-- | Settings related to your DASH output package. For more information, see
-- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/outputs-file-ABR.html.
-- When you work directly in your JSON job specification, include this
-- object and any required children when you set Type, under
-- OutputGroupSettings, to DASH_ISO_GROUP_SETTINGS.
outputGroupSettings_dashIsoGroupSettings :: Lens.Lens' OutputGroupSettings (Prelude.Maybe DashIsoGroupSettings)
outputGroupSettings_dashIsoGroupSettings = Lens.lens (\OutputGroupSettings' {dashIsoGroupSettings} -> dashIsoGroupSettings) (\s@OutputGroupSettings' {} a -> s {dashIsoGroupSettings = a} :: OutputGroupSettings)

-- | Settings related to your File output group. MediaConvert uses this group
-- of settings to generate a single standalone file, rather than a
-- streaming package. When you work directly in your JSON job
-- specification, include this object and any required children when you
-- set Type, under OutputGroupSettings, to FILE_GROUP_SETTINGS.
outputGroupSettings_fileGroupSettings :: Lens.Lens' OutputGroupSettings (Prelude.Maybe FileGroupSettings)
outputGroupSettings_fileGroupSettings = Lens.lens (\OutputGroupSettings' {fileGroupSettings} -> fileGroupSettings) (\s@OutputGroupSettings' {} a -> s {fileGroupSettings = a} :: OutputGroupSettings)

-- | Settings related to your HLS output package. For more information, see
-- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/outputs-file-ABR.html.
-- When you work directly in your JSON job specification, include this
-- object and any required children when you set Type, under
-- OutputGroupSettings, to HLS_GROUP_SETTINGS.
outputGroupSettings_hlsGroupSettings :: Lens.Lens' OutputGroupSettings (Prelude.Maybe HlsGroupSettings)
outputGroupSettings_hlsGroupSettings = Lens.lens (\OutputGroupSettings' {hlsGroupSettings} -> hlsGroupSettings) (\s@OutputGroupSettings' {} a -> s {hlsGroupSettings = a} :: OutputGroupSettings)

-- | Settings related to your Microsoft Smooth Streaming output package. For
-- more information, see
-- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/outputs-file-ABR.html.
-- When you work directly in your JSON job specification, include this
-- object and any required children when you set Type, under
-- OutputGroupSettings, to MS_SMOOTH_GROUP_SETTINGS.
outputGroupSettings_msSmoothGroupSettings :: Lens.Lens' OutputGroupSettings (Prelude.Maybe MsSmoothGroupSettings)
outputGroupSettings_msSmoothGroupSettings = Lens.lens (\OutputGroupSettings' {msSmoothGroupSettings} -> msSmoothGroupSettings) (\s@OutputGroupSettings' {} a -> s {msSmoothGroupSettings = a} :: OutputGroupSettings)

-- | Type of output group (File group, Apple HLS, DASH ISO, Microsoft Smooth
-- Streaming, CMAF)
outputGroupSettings_type :: Lens.Lens' OutputGroupSettings (Prelude.Maybe OutputGroupType)
outputGroupSettings_type = Lens.lens (\OutputGroupSettings' {type'} -> type') (\s@OutputGroupSettings' {} a -> s {type' = a} :: OutputGroupSettings)

instance Data.FromJSON OutputGroupSettings where
  parseJSON =
    Data.withObject
      "OutputGroupSettings"
      ( \x ->
          OutputGroupSettings'
            Prelude.<$> (x Data..:? "cmafGroupSettings")
            Prelude.<*> (x Data..:? "dashIsoGroupSettings")
            Prelude.<*> (x Data..:? "fileGroupSettings")
            Prelude.<*> (x Data..:? "hlsGroupSettings")
            Prelude.<*> (x Data..:? "msSmoothGroupSettings")
            Prelude.<*> (x Data..:? "type")
      )

instance Prelude.Hashable OutputGroupSettings where
  hashWithSalt _salt OutputGroupSettings' {..} =
    _salt
      `Prelude.hashWithSalt` cmafGroupSettings
      `Prelude.hashWithSalt` dashIsoGroupSettings
      `Prelude.hashWithSalt` fileGroupSettings
      `Prelude.hashWithSalt` hlsGroupSettings
      `Prelude.hashWithSalt` msSmoothGroupSettings
      `Prelude.hashWithSalt` type'

instance Prelude.NFData OutputGroupSettings where
  rnf OutputGroupSettings' {..} =
    Prelude.rnf cmafGroupSettings
      `Prelude.seq` Prelude.rnf dashIsoGroupSettings
      `Prelude.seq` Prelude.rnf fileGroupSettings
      `Prelude.seq` Prelude.rnf hlsGroupSettings
      `Prelude.seq` Prelude.rnf msSmoothGroupSettings
      `Prelude.seq` Prelude.rnf type'

instance Data.ToJSON OutputGroupSettings where
  toJSON OutputGroupSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("cmafGroupSettings" Data..=)
              Prelude.<$> cmafGroupSettings,
            ("dashIsoGroupSettings" Data..=)
              Prelude.<$> dashIsoGroupSettings,
            ("fileGroupSettings" Data..=)
              Prelude.<$> fileGroupSettings,
            ("hlsGroupSettings" Data..=)
              Prelude.<$> hlsGroupSettings,
            ("msSmoothGroupSettings" Data..=)
              Prelude.<$> msSmoothGroupSettings,
            ("type" Data..=) Prelude.<$> type'
          ]
      )
