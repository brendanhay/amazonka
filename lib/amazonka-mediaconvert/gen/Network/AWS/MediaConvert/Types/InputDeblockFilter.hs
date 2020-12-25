{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.InputDeblockFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.InputDeblockFilter
  ( InputDeblockFilter
      ( InputDeblockFilter',
        InputDeblockFilterEnabled,
        InputDeblockFilterDisabled,
        fromInputDeblockFilter
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | Enable Deblock (InputDeblockFilter) to produce smoother motion in the output. Default is disabled. Only manually controllable for MPEG2 and uncompressed video inputs.
newtype InputDeblockFilter = InputDeblockFilter'
  { fromInputDeblockFilter ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern InputDeblockFilterEnabled :: InputDeblockFilter
pattern InputDeblockFilterEnabled = InputDeblockFilter' "ENABLED"

pattern InputDeblockFilterDisabled :: InputDeblockFilter
pattern InputDeblockFilterDisabled = InputDeblockFilter' "DISABLED"

{-# COMPLETE
  InputDeblockFilterEnabled,
  InputDeblockFilterDisabled,
  InputDeblockFilter'
  #-}
