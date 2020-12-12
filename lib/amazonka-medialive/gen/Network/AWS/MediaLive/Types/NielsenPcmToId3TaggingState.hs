{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.NielsenPcmToId3TaggingState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.NielsenPcmToId3TaggingState
  ( NielsenPcmToId3TaggingState
      ( NielsenPcmToId3TaggingState',
        NPTITSDisabled,
        NPTITSEnabled
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | State of Nielsen PCM to ID3 tagging
newtype NielsenPcmToId3TaggingState = NielsenPcmToId3TaggingState' Lude.Text
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

pattern NPTITSDisabled :: NielsenPcmToId3TaggingState
pattern NPTITSDisabled = NielsenPcmToId3TaggingState' "DISABLED"

pattern NPTITSEnabled :: NielsenPcmToId3TaggingState
pattern NPTITSEnabled = NielsenPcmToId3TaggingState' "ENABLED"

{-# COMPLETE
  NPTITSDisabled,
  NPTITSEnabled,
  NielsenPcmToId3TaggingState'
  #-}
