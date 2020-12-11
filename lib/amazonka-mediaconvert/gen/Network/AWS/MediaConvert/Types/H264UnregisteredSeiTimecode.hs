-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.H264UnregisteredSeiTimecode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.H264UnregisteredSeiTimecode
  ( H264UnregisteredSeiTimecode
      ( H264UnregisteredSeiTimecode',
        HDisabled,
        HEnabled
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Inserts timecode for each frame as 4 bytes of an unregistered SEI message.
newtype H264UnregisteredSeiTimecode = H264UnregisteredSeiTimecode' Lude.Text
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

pattern HDisabled :: H264UnregisteredSeiTimecode
pattern HDisabled = H264UnregisteredSeiTimecode' "DISABLED"

pattern HEnabled :: H264UnregisteredSeiTimecode
pattern HEnabled = H264UnregisteredSeiTimecode' "ENABLED"

{-# COMPLETE
  HDisabled,
  HEnabled,
  H264UnregisteredSeiTimecode'
  #-}
