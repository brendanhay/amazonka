{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Mp4MoovPlacement
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Mp4MoovPlacement
  ( Mp4MoovPlacement
      ( Mp4MoovPlacement',
        MMPNormal,
        MMPProgressiveDownload
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | If set to PROGRESSIVE_DOWNLOAD, the MOOV atom is relocated to the beginning of the archive as required for progressive downloading. Otherwise it is placed normally at the end.
newtype Mp4MoovPlacement = Mp4MoovPlacement' Lude.Text
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

pattern MMPNormal :: Mp4MoovPlacement
pattern MMPNormal = Mp4MoovPlacement' "NORMAL"

pattern MMPProgressiveDownload :: Mp4MoovPlacement
pattern MMPProgressiveDownload = Mp4MoovPlacement' "PROGRESSIVE_DOWNLOAD"

{-# COMPLETE
  MMPNormal,
  MMPProgressiveDownload,
  Mp4MoovPlacement'
  #-}
