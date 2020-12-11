-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisVideoArchivedMedia.Types.DASHPlaybackMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisVideoArchivedMedia.Types.DASHPlaybackMode
  ( DASHPlaybackMode
      ( DASHPlaybackMode',
        DASHPMLive,
        DASHPMLiveReplay,
        DASHPMOnDemand
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype DASHPlaybackMode = DASHPlaybackMode' Lude.Text
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

pattern DASHPMLive :: DASHPlaybackMode
pattern DASHPMLive = DASHPlaybackMode' "LIVE"

pattern DASHPMLiveReplay :: DASHPlaybackMode
pattern DASHPMLiveReplay = DASHPlaybackMode' "LIVE_REPLAY"

pattern DASHPMOnDemand :: DASHPlaybackMode
pattern DASHPMOnDemand = DASHPlaybackMode' "ON_DEMAND"

{-# COMPLETE
  DASHPMLive,
  DASHPMLiveReplay,
  DASHPMOnDemand,
  DASHPlaybackMode'
  #-}
