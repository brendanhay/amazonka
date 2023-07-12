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
-- Module      : Amazonka.MediaConvert.Types.Xavc4kIntraCbgProfileSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.Xavc4kIntraCbgProfileSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConvert.Types.Xavc4kIntraCbgProfileClass
import qualified Amazonka.Prelude as Prelude

-- | Required when you set (Profile) under
-- (VideoDescription)>(CodecSettings)>(XavcSettings) to the value
-- XAVC_4K_INTRA_CBG.
--
-- /See:/ 'newXavc4kIntraCbgProfileSettings' smart constructor.
data Xavc4kIntraCbgProfileSettings = Xavc4kIntraCbgProfileSettings'
  { -- | Specify the XAVC Intra 4k (CBG) Class to set the bitrate of your output.
    -- Outputs of the same class have similar image quality over the operating
    -- points that are valid for that class.
    xavcClass :: Prelude.Maybe Xavc4kIntraCbgProfileClass
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Xavc4kIntraCbgProfileSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'xavcClass', 'xavc4kIntraCbgProfileSettings_xavcClass' - Specify the XAVC Intra 4k (CBG) Class to set the bitrate of your output.
-- Outputs of the same class have similar image quality over the operating
-- points that are valid for that class.
newXavc4kIntraCbgProfileSettings ::
  Xavc4kIntraCbgProfileSettings
newXavc4kIntraCbgProfileSettings =
  Xavc4kIntraCbgProfileSettings'
    { xavcClass =
        Prelude.Nothing
    }

-- | Specify the XAVC Intra 4k (CBG) Class to set the bitrate of your output.
-- Outputs of the same class have similar image quality over the operating
-- points that are valid for that class.
xavc4kIntraCbgProfileSettings_xavcClass :: Lens.Lens' Xavc4kIntraCbgProfileSettings (Prelude.Maybe Xavc4kIntraCbgProfileClass)
xavc4kIntraCbgProfileSettings_xavcClass = Lens.lens (\Xavc4kIntraCbgProfileSettings' {xavcClass} -> xavcClass) (\s@Xavc4kIntraCbgProfileSettings' {} a -> s {xavcClass = a} :: Xavc4kIntraCbgProfileSettings)

instance Data.FromJSON Xavc4kIntraCbgProfileSettings where
  parseJSON =
    Data.withObject
      "Xavc4kIntraCbgProfileSettings"
      ( \x ->
          Xavc4kIntraCbgProfileSettings'
            Prelude.<$> (x Data..:? "xavcClass")
      )

instance
  Prelude.Hashable
    Xavc4kIntraCbgProfileSettings
  where
  hashWithSalt _salt Xavc4kIntraCbgProfileSettings' {..} =
    _salt `Prelude.hashWithSalt` xavcClass

instance Prelude.NFData Xavc4kIntraCbgProfileSettings where
  rnf Xavc4kIntraCbgProfileSettings' {..} =
    Prelude.rnf xavcClass

instance Data.ToJSON Xavc4kIntraCbgProfileSettings where
  toJSON Xavc4kIntraCbgProfileSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [("xavcClass" Data..=) Prelude.<$> xavcClass]
      )
