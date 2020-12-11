-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.H264ForceFieldPictures
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.H264ForceFieldPictures
  ( H264ForceFieldPictures
      ( H264ForceFieldPictures',
        HFFPDisabled,
        HFFPEnabled
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | H264 Force Field Pictures
newtype H264ForceFieldPictures = H264ForceFieldPictures' Lude.Text
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

pattern HFFPDisabled :: H264ForceFieldPictures
pattern HFFPDisabled = H264ForceFieldPictures' "DISABLED"

pattern HFFPEnabled :: H264ForceFieldPictures
pattern HFFPEnabled = H264ForceFieldPictures' "ENABLED"

{-# COMPLETE
  HFFPDisabled,
  HFFPEnabled,
  H264ForceFieldPictures'
  #-}
