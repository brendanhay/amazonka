{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.ColorSpaceUsage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.ColorSpaceUsage
  ( ColorSpaceUsage
      ( ..,
        ColorSpaceUsage_FALLBACK,
        ColorSpaceUsage_FORCE
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

-- | There are two sources for color metadata, the input file and the job
-- input settings Color space (ColorSpace) and HDR master display
-- information settings(Hdr10Metadata). The Color space usage setting
-- determines which takes precedence. Choose Force (FORCE) to use color
-- metadata from the input job settings. If you don\'t specify values for
-- those settings, the service defaults to using metadata from your input.
-- FALLBACK - Choose Fallback (FALLBACK) to use color metadata from the
-- source when it is present. If there\'s no color metadata in your input
-- file, the service defaults to using values you specify in the input
-- settings.
newtype ColorSpaceUsage = ColorSpaceUsage'
  { fromColorSpaceUsage ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
    )

pattern ColorSpaceUsage_FALLBACK :: ColorSpaceUsage
pattern ColorSpaceUsage_FALLBACK = ColorSpaceUsage' "FALLBACK"

pattern ColorSpaceUsage_FORCE :: ColorSpaceUsage
pattern ColorSpaceUsage_FORCE = ColorSpaceUsage' "FORCE"

{-# COMPLETE
  ColorSpaceUsage_FALLBACK,
  ColorSpaceUsage_FORCE,
  ColorSpaceUsage'
  #-}
