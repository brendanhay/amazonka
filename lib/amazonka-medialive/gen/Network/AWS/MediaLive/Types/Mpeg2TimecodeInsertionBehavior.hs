-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Mpeg2TimecodeInsertionBehavior
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Mpeg2TimecodeInsertionBehavior
  ( Mpeg2TimecodeInsertionBehavior
      ( Mpeg2TimecodeInsertionBehavior',
        MTIBDisabled,
        MTIBGopTimecode
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Mpeg2 Timecode Insertion Behavior
newtype Mpeg2TimecodeInsertionBehavior = Mpeg2TimecodeInsertionBehavior' Lude.Text
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

pattern MTIBDisabled :: Mpeg2TimecodeInsertionBehavior
pattern MTIBDisabled = Mpeg2TimecodeInsertionBehavior' "DISABLED"

pattern MTIBGopTimecode :: Mpeg2TimecodeInsertionBehavior
pattern MTIBGopTimecode = Mpeg2TimecodeInsertionBehavior' "GOP_TIMECODE"

{-# COMPLETE
  MTIBDisabled,
  MTIBGopTimecode,
  Mpeg2TimecodeInsertionBehavior'
  #-}
