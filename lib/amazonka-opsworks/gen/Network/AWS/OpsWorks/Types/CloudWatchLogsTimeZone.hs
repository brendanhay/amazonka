{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.CloudWatchLogsTimeZone
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorks.Types.CloudWatchLogsTimeZone
  ( CloudWatchLogsTimeZone
      ( CloudWatchLogsTimeZone',
        Local,
        Utc
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | The preferred time zone for logs streamed to CloudWatch Logs. Valid values are @LOCAL@ and @UTC@ , for Coordinated Universal Time.
newtype CloudWatchLogsTimeZone = CloudWatchLogsTimeZone' Lude.Text
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

pattern Local :: CloudWatchLogsTimeZone
pattern Local = CloudWatchLogsTimeZone' "LOCAL"

pattern Utc :: CloudWatchLogsTimeZone
pattern Utc = CloudWatchLogsTimeZone' "UTC"

{-# COMPLETE
  Local,
  Utc,
  CloudWatchLogsTimeZone'
  #-}
