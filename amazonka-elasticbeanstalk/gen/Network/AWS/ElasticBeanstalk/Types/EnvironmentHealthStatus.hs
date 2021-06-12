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
-- Module      : Network.AWS.ElasticBeanstalk.Types.EnvironmentHealthStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.EnvironmentHealthStatus
  ( EnvironmentHealthStatus
      ( ..,
        EnvironmentHealthStatus_Degraded,
        EnvironmentHealthStatus_Info,
        EnvironmentHealthStatus_NoData,
        EnvironmentHealthStatus_Ok,
        EnvironmentHealthStatus_Pending,
        EnvironmentHealthStatus_Severe,
        EnvironmentHealthStatus_Suspended,
        EnvironmentHealthStatus_Unknown,
        EnvironmentHealthStatus_Warning
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype EnvironmentHealthStatus = EnvironmentHealthStatus'
  { fromEnvironmentHealthStatus ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
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

pattern EnvironmentHealthStatus_Degraded :: EnvironmentHealthStatus
pattern EnvironmentHealthStatus_Degraded = EnvironmentHealthStatus' "Degraded"

pattern EnvironmentHealthStatus_Info :: EnvironmentHealthStatus
pattern EnvironmentHealthStatus_Info = EnvironmentHealthStatus' "Info"

pattern EnvironmentHealthStatus_NoData :: EnvironmentHealthStatus
pattern EnvironmentHealthStatus_NoData = EnvironmentHealthStatus' "NoData"

pattern EnvironmentHealthStatus_Ok :: EnvironmentHealthStatus
pattern EnvironmentHealthStatus_Ok = EnvironmentHealthStatus' "Ok"

pattern EnvironmentHealthStatus_Pending :: EnvironmentHealthStatus
pattern EnvironmentHealthStatus_Pending = EnvironmentHealthStatus' "Pending"

pattern EnvironmentHealthStatus_Severe :: EnvironmentHealthStatus
pattern EnvironmentHealthStatus_Severe = EnvironmentHealthStatus' "Severe"

pattern EnvironmentHealthStatus_Suspended :: EnvironmentHealthStatus
pattern EnvironmentHealthStatus_Suspended = EnvironmentHealthStatus' "Suspended"

pattern EnvironmentHealthStatus_Unknown :: EnvironmentHealthStatus
pattern EnvironmentHealthStatus_Unknown = EnvironmentHealthStatus' "Unknown"

pattern EnvironmentHealthStatus_Warning :: EnvironmentHealthStatus
pattern EnvironmentHealthStatus_Warning = EnvironmentHealthStatus' "Warning"

{-# COMPLETE
  EnvironmentHealthStatus_Degraded,
  EnvironmentHealthStatus_Info,
  EnvironmentHealthStatus_NoData,
  EnvironmentHealthStatus_Ok,
  EnvironmentHealthStatus_Pending,
  EnvironmentHealthStatus_Severe,
  EnvironmentHealthStatus_Suspended,
  EnvironmentHealthStatus_Unknown,
  EnvironmentHealthStatus_Warning,
  EnvironmentHealthStatus'
  #-}
