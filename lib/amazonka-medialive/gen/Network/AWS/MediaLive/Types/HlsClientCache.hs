-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.HlsClientCache
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.HlsClientCache
  ( HlsClientCache
      ( HlsClientCache',
        HCCDisabled,
        HCCEnabled
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Hls Client Cache
newtype HlsClientCache = HlsClientCache' Lude.Text
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

pattern HCCDisabled :: HlsClientCache
pattern HCCDisabled = HlsClientCache' "DISABLED"

pattern HCCEnabled :: HlsClientCache
pattern HCCEnabled = HlsClientCache' "ENABLED"

{-# COMPLETE
  HCCDisabled,
  HCCEnabled,
  HlsClientCache'
  #-}
