{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.H265UnregisteredSeiTimecode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.H265UnregisteredSeiTimecode
  ( H265UnregisteredSeiTimecode
      ( H265UnregisteredSeiTimecode',
        HUSTDisabled,
        HUSTEnabled
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Inserts timecode for each frame as 4 bytes of an unregistered SEI message.
newtype H265UnregisteredSeiTimecode = H265UnregisteredSeiTimecode' Lude.Text
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

pattern HUSTDisabled :: H265UnregisteredSeiTimecode
pattern HUSTDisabled = H265UnregisteredSeiTimecode' "DISABLED"

pattern HUSTEnabled :: H265UnregisteredSeiTimecode
pattern HUSTEnabled = H265UnregisteredSeiTimecode' "ENABLED"

{-# COMPLETE
  HUSTDisabled,
  HUSTEnabled,
  H265UnregisteredSeiTimecode'
  #-}
