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
-- Module      : Network.AWS.ElasticBeanstalk.Types.InstancesHealthAttribute
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.InstancesHealthAttribute
  ( InstancesHealthAttribute
      ( ..,
        InstancesHealthAttribute_All,
        InstancesHealthAttribute_ApplicationMetrics,
        InstancesHealthAttribute_AvailabilityZone,
        InstancesHealthAttribute_Causes,
        InstancesHealthAttribute_Color,
        InstancesHealthAttribute_Deployment,
        InstancesHealthAttribute_HealthStatus,
        InstancesHealthAttribute_InstanceType,
        InstancesHealthAttribute_LaunchedAt,
        InstancesHealthAttribute_RefreshedAt,
        InstancesHealthAttribute_System
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype InstancesHealthAttribute = InstancesHealthAttribute'
  { fromInstancesHealthAttribute ::
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

pattern InstancesHealthAttribute_All :: InstancesHealthAttribute
pattern InstancesHealthAttribute_All = InstancesHealthAttribute' "All"

pattern InstancesHealthAttribute_ApplicationMetrics :: InstancesHealthAttribute
pattern InstancesHealthAttribute_ApplicationMetrics = InstancesHealthAttribute' "ApplicationMetrics"

pattern InstancesHealthAttribute_AvailabilityZone :: InstancesHealthAttribute
pattern InstancesHealthAttribute_AvailabilityZone = InstancesHealthAttribute' "AvailabilityZone"

pattern InstancesHealthAttribute_Causes :: InstancesHealthAttribute
pattern InstancesHealthAttribute_Causes = InstancesHealthAttribute' "Causes"

pattern InstancesHealthAttribute_Color :: InstancesHealthAttribute
pattern InstancesHealthAttribute_Color = InstancesHealthAttribute' "Color"

pattern InstancesHealthAttribute_Deployment :: InstancesHealthAttribute
pattern InstancesHealthAttribute_Deployment = InstancesHealthAttribute' "Deployment"

pattern InstancesHealthAttribute_HealthStatus :: InstancesHealthAttribute
pattern InstancesHealthAttribute_HealthStatus = InstancesHealthAttribute' "HealthStatus"

pattern InstancesHealthAttribute_InstanceType :: InstancesHealthAttribute
pattern InstancesHealthAttribute_InstanceType = InstancesHealthAttribute' "InstanceType"

pattern InstancesHealthAttribute_LaunchedAt :: InstancesHealthAttribute
pattern InstancesHealthAttribute_LaunchedAt = InstancesHealthAttribute' "LaunchedAt"

pattern InstancesHealthAttribute_RefreshedAt :: InstancesHealthAttribute
pattern InstancesHealthAttribute_RefreshedAt = InstancesHealthAttribute' "RefreshedAt"

pattern InstancesHealthAttribute_System :: InstancesHealthAttribute
pattern InstancesHealthAttribute_System = InstancesHealthAttribute' "System"

{-# COMPLETE
  InstancesHealthAttribute_All,
  InstancesHealthAttribute_ApplicationMetrics,
  InstancesHealthAttribute_AvailabilityZone,
  InstancesHealthAttribute_Causes,
  InstancesHealthAttribute_Color,
  InstancesHealthAttribute_Deployment,
  InstancesHealthAttribute_HealthStatus,
  InstancesHealthAttribute_InstanceType,
  InstancesHealthAttribute_LaunchedAt,
  InstancesHealthAttribute_RefreshedAt,
  InstancesHealthAttribute_System,
  InstancesHealthAttribute'
  #-}
