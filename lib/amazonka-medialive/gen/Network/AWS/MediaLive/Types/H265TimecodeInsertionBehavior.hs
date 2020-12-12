{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.H265TimecodeInsertionBehavior
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.H265TimecodeInsertionBehavior
  ( H265TimecodeInsertionBehavior
      ( H265TimecodeInsertionBehavior',
        HTIBDisabled,
        HTIBPicTimingSei
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | H265 Timecode Insertion Behavior
newtype H265TimecodeInsertionBehavior = H265TimecodeInsertionBehavior' Lude.Text
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

pattern HTIBDisabled :: H265TimecodeInsertionBehavior
pattern HTIBDisabled = H265TimecodeInsertionBehavior' "DISABLED"

pattern HTIBPicTimingSei :: H265TimecodeInsertionBehavior
pattern HTIBPicTimingSei = H265TimecodeInsertionBehavior' "PIC_TIMING_SEI"

{-# COMPLETE
  HTIBDisabled,
  HTIBPicTimingSei,
  H265TimecodeInsertionBehavior'
  #-}
