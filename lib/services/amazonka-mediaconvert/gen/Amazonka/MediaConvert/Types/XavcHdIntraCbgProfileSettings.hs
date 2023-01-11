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
-- Module      : Amazonka.MediaConvert.Types.XavcHdIntraCbgProfileSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.XavcHdIntraCbgProfileSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConvert.Types.XavcHdIntraCbgProfileClass
import qualified Amazonka.Prelude as Prelude

-- | Required when you set (Profile) under
-- (VideoDescription)>(CodecSettings)>(XavcSettings) to the value
-- XAVC_HD_INTRA_CBG.
--
-- /See:/ 'newXavcHdIntraCbgProfileSettings' smart constructor.
data XavcHdIntraCbgProfileSettings = XavcHdIntraCbgProfileSettings'
  { -- | Specify the XAVC Intra HD (CBG) Class to set the bitrate of your output.
    -- Outputs of the same class have similar image quality over the operating
    -- points that are valid for that class.
    xavcClass :: Prelude.Maybe XavcHdIntraCbgProfileClass
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'XavcHdIntraCbgProfileSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'xavcClass', 'xavcHdIntraCbgProfileSettings_xavcClass' - Specify the XAVC Intra HD (CBG) Class to set the bitrate of your output.
-- Outputs of the same class have similar image quality over the operating
-- points that are valid for that class.
newXavcHdIntraCbgProfileSettings ::
  XavcHdIntraCbgProfileSettings
newXavcHdIntraCbgProfileSettings =
  XavcHdIntraCbgProfileSettings'
    { xavcClass =
        Prelude.Nothing
    }

-- | Specify the XAVC Intra HD (CBG) Class to set the bitrate of your output.
-- Outputs of the same class have similar image quality over the operating
-- points that are valid for that class.
xavcHdIntraCbgProfileSettings_xavcClass :: Lens.Lens' XavcHdIntraCbgProfileSettings (Prelude.Maybe XavcHdIntraCbgProfileClass)
xavcHdIntraCbgProfileSettings_xavcClass = Lens.lens (\XavcHdIntraCbgProfileSettings' {xavcClass} -> xavcClass) (\s@XavcHdIntraCbgProfileSettings' {} a -> s {xavcClass = a} :: XavcHdIntraCbgProfileSettings)

instance Data.FromJSON XavcHdIntraCbgProfileSettings where
  parseJSON =
    Data.withObject
      "XavcHdIntraCbgProfileSettings"
      ( \x ->
          XavcHdIntraCbgProfileSettings'
            Prelude.<$> (x Data..:? "xavcClass")
      )

instance
  Prelude.Hashable
    XavcHdIntraCbgProfileSettings
  where
  hashWithSalt _salt XavcHdIntraCbgProfileSettings' {..} =
    _salt `Prelude.hashWithSalt` xavcClass

instance Prelude.NFData XavcHdIntraCbgProfileSettings where
  rnf XavcHdIntraCbgProfileSettings' {..} =
    Prelude.rnf xavcClass

instance Data.ToJSON XavcHdIntraCbgProfileSettings where
  toJSON XavcHdIntraCbgProfileSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [("xavcClass" Data..=) Prelude.<$> xavcClass]
      )
