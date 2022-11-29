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
-- Module      : Amazonka.MediaLive.Types.ColorSpacePassthroughSettings
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.ColorSpacePassthroughSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Passthrough applies no color space conversion to the output
--
-- /See:/ 'newColorSpacePassthroughSettings' smart constructor.
data ColorSpacePassthroughSettings = ColorSpacePassthroughSettings'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ColorSpacePassthroughSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newColorSpacePassthroughSettings ::
  ColorSpacePassthroughSettings
newColorSpacePassthroughSettings =
  ColorSpacePassthroughSettings'

instance Core.FromJSON ColorSpacePassthroughSettings where
  parseJSON =
    Core.withObject
      "ColorSpacePassthroughSettings"
      (\x -> Prelude.pure ColorSpacePassthroughSettings')

instance
  Prelude.Hashable
    ColorSpacePassthroughSettings
  where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance Prelude.NFData ColorSpacePassthroughSettings where
  rnf _ = ()

instance Core.ToJSON ColorSpacePassthroughSettings where
  toJSON = Prelude.const (Core.Object Prelude.mempty)
