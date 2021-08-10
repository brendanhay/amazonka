{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.ScheduledAutoTuneSeverityType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.ScheduledAutoTuneSeverityType
  ( ScheduledAutoTuneSeverityType
      ( ..,
        ScheduledAutoTuneSeverityType_HIGH,
        ScheduledAutoTuneSeverityType_LOW,
        ScheduledAutoTuneSeverityType_MEDIUM
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

-- | Specifies Auto-Tune action severity. Valid values are LOW, MEDIUM and
-- HIGH.
newtype ScheduledAutoTuneSeverityType = ScheduledAutoTuneSeverityType'
  { fromScheduledAutoTuneSeverityType ::
      Core.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
    )

pattern ScheduledAutoTuneSeverityType_HIGH :: ScheduledAutoTuneSeverityType
pattern ScheduledAutoTuneSeverityType_HIGH = ScheduledAutoTuneSeverityType' "HIGH"

pattern ScheduledAutoTuneSeverityType_LOW :: ScheduledAutoTuneSeverityType
pattern ScheduledAutoTuneSeverityType_LOW = ScheduledAutoTuneSeverityType' "LOW"

pattern ScheduledAutoTuneSeverityType_MEDIUM :: ScheduledAutoTuneSeverityType
pattern ScheduledAutoTuneSeverityType_MEDIUM = ScheduledAutoTuneSeverityType' "MEDIUM"

{-# COMPLETE
  ScheduledAutoTuneSeverityType_HIGH,
  ScheduledAutoTuneSeverityType_LOW,
  ScheduledAutoTuneSeverityType_MEDIUM,
  ScheduledAutoTuneSeverityType'
  #-}
