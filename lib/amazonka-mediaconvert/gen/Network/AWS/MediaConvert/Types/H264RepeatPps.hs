-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.H264RepeatPps
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.H264RepeatPps
  ( H264RepeatPps
      ( H264RepeatPps',
        HRPDisabled,
        HRPEnabled
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Places a PPS header on each encoded picture, even if repeated.
newtype H264RepeatPps = H264RepeatPps' Lude.Text
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

pattern HRPDisabled :: H264RepeatPps
pattern HRPDisabled = H264RepeatPps' "DISABLED"

pattern HRPEnabled :: H264RepeatPps
pattern HRPEnabled = H264RepeatPps' "ENABLED"

{-# COMPLETE
  HRPDisabled,
  HRPEnabled,
  H264RepeatPps'
  #-}
