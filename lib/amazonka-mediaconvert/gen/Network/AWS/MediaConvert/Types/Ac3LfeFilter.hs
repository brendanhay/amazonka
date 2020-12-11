-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Ac3LfeFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Ac3LfeFilter
  ( Ac3LfeFilter
      ( Ac3LfeFilter',
        ALFDisabled,
        ALFEnabled
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Applies a 120Hz lowpass filter to the LFE channel prior to encoding. Only valid with 3_2_LFE coding mode.
newtype Ac3LfeFilter = Ac3LfeFilter' Lude.Text
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

pattern ALFDisabled :: Ac3LfeFilter
pattern ALFDisabled = Ac3LfeFilter' "DISABLED"

pattern ALFEnabled :: Ac3LfeFilter
pattern ALFEnabled = Ac3LfeFilter' "ENABLED"

{-# COMPLETE
  ALFDisabled,
  ALFEnabled,
  Ac3LfeFilter'
  #-}
