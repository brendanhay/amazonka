-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.Types.AlarmType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatch.Types.AlarmType
  ( AlarmType
      ( AlarmType',
        CompositeAlarm,
        MetricAlarm
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype AlarmType = AlarmType' Lude.Text
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

pattern CompositeAlarm :: AlarmType
pattern CompositeAlarm = AlarmType' "CompositeAlarm"

pattern MetricAlarm :: AlarmType
pattern MetricAlarm = AlarmType' "MetricAlarm"

{-# COMPLETE
  CompositeAlarm,
  MetricAlarm,
  AlarmType'
  #-}
