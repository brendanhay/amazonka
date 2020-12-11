-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.H264SpatialAq
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.H264SpatialAq
  ( H264SpatialAq
      ( H264SpatialAq',
        HSADisabled,
        HSAEnabled
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | H264 Spatial Aq
newtype H264SpatialAq = H264SpatialAq' Lude.Text
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

pattern HSADisabled :: H264SpatialAq
pattern HSADisabled = H264SpatialAq' "DISABLED"

pattern HSAEnabled :: H264SpatialAq
pattern HSAEnabled = H264SpatialAq' "ENABLED"

{-# COMPLETE
  HSADisabled,
  HSAEnabled,
  H264SpatialAq'
  #-}
