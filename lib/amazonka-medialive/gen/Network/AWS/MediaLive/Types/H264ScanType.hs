{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.H264ScanType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.H264ScanType
  ( H264ScanType
      ( H264ScanType',
        H264ScanTypeInterlaced,
        H264ScanTypeProgressive,
        fromH264ScanType
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | H264 Scan Type
newtype H264ScanType = H264ScanType' {fromH264ScanType :: Core.Text}
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

pattern H264ScanTypeInterlaced :: H264ScanType
pattern H264ScanTypeInterlaced = H264ScanType' "INTERLACED"

pattern H264ScanTypeProgressive :: H264ScanType
pattern H264ScanTypeProgressive = H264ScanType' "PROGRESSIVE"

{-# COMPLETE
  H264ScanTypeInterlaced,
  H264ScanTypeProgressive,
  H264ScanType'
  #-}
