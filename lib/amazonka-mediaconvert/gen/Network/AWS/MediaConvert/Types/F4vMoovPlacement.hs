-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.F4vMoovPlacement
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.F4vMoovPlacement
  ( F4vMoovPlacement
      ( F4vMoovPlacement',
        FMPNormal,
        FMPProgressiveDownload
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | If set to PROGRESSIVE_DOWNLOAD, the MOOV atom is relocated to the beginning of the archive as required for progressive downloading. Otherwise it is placed normally at the end.
newtype F4vMoovPlacement = F4vMoovPlacement' Lude.Text
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

pattern FMPNormal :: F4vMoovPlacement
pattern FMPNormal = F4vMoovPlacement' "NORMAL"

pattern FMPProgressiveDownload :: F4vMoovPlacement
pattern FMPProgressiveDownload = F4vMoovPlacement' "PROGRESSIVE_DOWNLOAD"

{-# COMPLETE
  FMPNormal,
  FMPProgressiveDownload,
  F4vMoovPlacement'
  #-}
