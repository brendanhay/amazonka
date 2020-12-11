-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.M2tsTimedMetadataBehavior
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.M2tsTimedMetadataBehavior
  ( M2tsTimedMetadataBehavior
      ( M2tsTimedMetadataBehavior',
        MTMBNoPassthrough,
        MTMBPassthrough
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | M2ts Timed Metadata Behavior
newtype M2tsTimedMetadataBehavior = M2tsTimedMetadataBehavior' Lude.Text
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

pattern MTMBNoPassthrough :: M2tsTimedMetadataBehavior
pattern MTMBNoPassthrough = M2tsTimedMetadataBehavior' "NO_PASSTHROUGH"

pattern MTMBPassthrough :: M2tsTimedMetadataBehavior
pattern MTMBPassthrough = M2tsTimedMetadataBehavior' "PASSTHROUGH"

{-# COMPLETE
  MTMBNoPassthrough,
  MTMBPassthrough,
  M2tsTimedMetadataBehavior'
  #-}
