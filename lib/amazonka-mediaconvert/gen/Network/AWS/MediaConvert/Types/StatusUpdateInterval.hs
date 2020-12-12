{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.StatusUpdateInterval
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.StatusUpdateInterval
  ( StatusUpdateInterval
      ( StatusUpdateInterval',
        Seconds10,
        Seconds12,
        Seconds120,
        Seconds15,
        Seconds180,
        Seconds20,
        Seconds240,
        Seconds30,
        Seconds300,
        Seconds360,
        Seconds420,
        Seconds480,
        Seconds540,
        Seconds60,
        Seconds600
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Specify how often MediaConvert sends STATUS_UPDATE events to Amazon CloudWatch Events. Set the interval, in seconds, between status updates. MediaConvert sends an update at this interval from the time the service begins processing your job to the time it completes the transcode or encounters an error.
newtype StatusUpdateInterval = StatusUpdateInterval' Lude.Text
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

pattern Seconds10 :: StatusUpdateInterval
pattern Seconds10 = StatusUpdateInterval' "SECONDS_10"

pattern Seconds12 :: StatusUpdateInterval
pattern Seconds12 = StatusUpdateInterval' "SECONDS_12"

pattern Seconds120 :: StatusUpdateInterval
pattern Seconds120 = StatusUpdateInterval' "SECONDS_120"

pattern Seconds15 :: StatusUpdateInterval
pattern Seconds15 = StatusUpdateInterval' "SECONDS_15"

pattern Seconds180 :: StatusUpdateInterval
pattern Seconds180 = StatusUpdateInterval' "SECONDS_180"

pattern Seconds20 :: StatusUpdateInterval
pattern Seconds20 = StatusUpdateInterval' "SECONDS_20"

pattern Seconds240 :: StatusUpdateInterval
pattern Seconds240 = StatusUpdateInterval' "SECONDS_240"

pattern Seconds30 :: StatusUpdateInterval
pattern Seconds30 = StatusUpdateInterval' "SECONDS_30"

pattern Seconds300 :: StatusUpdateInterval
pattern Seconds300 = StatusUpdateInterval' "SECONDS_300"

pattern Seconds360 :: StatusUpdateInterval
pattern Seconds360 = StatusUpdateInterval' "SECONDS_360"

pattern Seconds420 :: StatusUpdateInterval
pattern Seconds420 = StatusUpdateInterval' "SECONDS_420"

pattern Seconds480 :: StatusUpdateInterval
pattern Seconds480 = StatusUpdateInterval' "SECONDS_480"

pattern Seconds540 :: StatusUpdateInterval
pattern Seconds540 = StatusUpdateInterval' "SECONDS_540"

pattern Seconds60 :: StatusUpdateInterval
pattern Seconds60 = StatusUpdateInterval' "SECONDS_60"

pattern Seconds600 :: StatusUpdateInterval
pattern Seconds600 = StatusUpdateInterval' "SECONDS_600"

{-# COMPLETE
  Seconds10,
  Seconds12,
  Seconds120,
  Seconds15,
  Seconds180,
  Seconds20,
  Seconds240,
  Seconds30,
  Seconds300,
  Seconds360,
  Seconds420,
  Seconds480,
  Seconds540,
  Seconds60,
  Seconds600,
  StatusUpdateInterval'
  #-}
