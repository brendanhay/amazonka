{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.HlsOfflineEncrypted
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.HlsOfflineEncrypted
  ( HlsOfflineEncrypted
      ( HlsOfflineEncrypted',
        HOEDisabled,
        HOEEnabled
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Enable this setting to insert the EXT-X-SESSION-KEY element into the master playlist. This allows for offline Apple HLS FairPlay content protection.
newtype HlsOfflineEncrypted = HlsOfflineEncrypted' Lude.Text
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

pattern HOEDisabled :: HlsOfflineEncrypted
pattern HOEDisabled = HlsOfflineEncrypted' "DISABLED"

pattern HOEEnabled :: HlsOfflineEncrypted
pattern HOEEnabled = HlsOfflineEncrypted' "ENABLED"

{-# COMPLETE
  HOEDisabled,
  HOEEnabled,
  HlsOfflineEncrypted'
  #-}
