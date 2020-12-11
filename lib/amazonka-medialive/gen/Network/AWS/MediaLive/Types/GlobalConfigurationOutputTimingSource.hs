-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.GlobalConfigurationOutputTimingSource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.GlobalConfigurationOutputTimingSource
  ( GlobalConfigurationOutputTimingSource
      ( GlobalConfigurationOutputTimingSource',
        InputClock,
        SystemClock
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Global Configuration Output Timing Source
newtype GlobalConfigurationOutputTimingSource = GlobalConfigurationOutputTimingSource' Lude.Text
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

pattern InputClock :: GlobalConfigurationOutputTimingSource
pattern InputClock = GlobalConfigurationOutputTimingSource' "INPUT_CLOCK"

pattern SystemClock :: GlobalConfigurationOutputTimingSource
pattern SystemClock = GlobalConfigurationOutputTimingSource' "SYSTEM_CLOCK"

{-# COMPLETE
  InputClock,
  SystemClock,
  GlobalConfigurationOutputTimingSource'
  #-}
