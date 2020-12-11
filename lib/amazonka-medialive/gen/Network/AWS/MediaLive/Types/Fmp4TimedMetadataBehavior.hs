-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Fmp4TimedMetadataBehavior
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Fmp4TimedMetadataBehavior
  ( Fmp4TimedMetadataBehavior
      ( Fmp4TimedMetadataBehavior',
        FTMBNoPassthrough,
        FTMBPassthrough
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Fmp4 Timed Metadata Behavior
newtype Fmp4TimedMetadataBehavior = Fmp4TimedMetadataBehavior' Lude.Text
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

pattern FTMBNoPassthrough :: Fmp4TimedMetadataBehavior
pattern FTMBNoPassthrough = Fmp4TimedMetadataBehavior' "NO_PASSTHROUGH"

pattern FTMBPassthrough :: Fmp4TimedMetadataBehavior
pattern FTMBPassthrough = Fmp4TimedMetadataBehavior' "PASSTHROUGH"

{-# COMPLETE
  FTMBNoPassthrough,
  FTMBPassthrough,
  Fmp4TimedMetadataBehavior'
  #-}
