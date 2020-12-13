{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.InputDenoiseFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.InputDenoiseFilter
  ( InputDenoiseFilter
      ( InputDenoiseFilter',
        IEnabled,
        IDisabled
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Enable Denoise (InputDenoiseFilter) to filter noise from the input.  Default is disabled. Only applicable to MPEG2, H.264, H.265, and uncompressed video inputs.
newtype InputDenoiseFilter = InputDenoiseFilter' Lude.Text
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

pattern IEnabled :: InputDenoiseFilter
pattern IEnabled = InputDenoiseFilter' "ENABLED"

pattern IDisabled :: InputDenoiseFilter
pattern IDisabled = InputDenoiseFilter' "DISABLED"

{-# COMPLETE
  IEnabled,
  IDisabled,
  InputDenoiseFilter'
  #-}
