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
        IDFDisabled,
        IDFEnabled
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Enable Deblock (InputDeblockFilter) to produce smoother motion in the output. Default is disabled. Only manually controllable for MPEG2 and uncompressed video inputs.
newtype InputDeblockFilter = InputDeblockFilter' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern IDFDisabled :: InputDeblockFilter
pattern IDFDisabled = InputDeblockFilter' "DISABLED"

pattern IDFEnabled :: InputDeblockFilter
pattern IDFEnabled = InputDeblockFilter' "ENABLED"

{-# COMPLETE
  IDFDisabled,
  IDFEnabled,
  InputDeblockFilter'
  #-}
