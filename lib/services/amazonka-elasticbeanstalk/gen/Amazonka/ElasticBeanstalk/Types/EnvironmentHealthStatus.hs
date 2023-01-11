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
-- Module      : Amazonka.ElasticBeanstalk.Types.EnvironmentHealthStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElasticBeanstalk.Types.EnvironmentHealthStatus
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype EnvironmentHealthStatus = EnvironmentHealthStatus'
  { fromEnvironmentHealthStatus ::
      Data.Text
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
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
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
