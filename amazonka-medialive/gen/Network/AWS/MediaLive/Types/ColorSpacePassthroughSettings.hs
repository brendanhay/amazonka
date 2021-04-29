{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.MediaLive.Types.ColorSpacePassthroughSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.ColorSpacePassthroughSettings where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Passthrough applies no color space conversion to the output
--
-- /See:/ 'newColorSpacePassthroughSettings' smart constructor.
data ColorSpacePassthroughSettings = ColorSpacePassthroughSettings'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ColorSpacePassthroughSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newColorSpacePassthroughSettings ::
  ColorSpacePassthroughSettings
newColorSpacePassthroughSettings =
  ColorSpacePassthroughSettings'

instance
  Prelude.FromJSON
    ColorSpacePassthroughSettings
  where
  parseJSON =
    Prelude.withObject
      "ColorSpacePassthroughSettings"
      (\x -> Prelude.pure ColorSpacePassthroughSettings')

instance
  Prelude.Hashable
    ColorSpacePassthroughSettings

instance Prelude.NFData ColorSpacePassthroughSettings

instance Prelude.ToJSON ColorSpacePassthroughSettings where
  toJSON =
    Prelude.const (Prelude.Object Prelude.mempty)
