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
-- Module      : Amazonka.ElasticBeanstalk.Types.EnvironmentHealthAttribute
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElasticBeanstalk.Types.EnvironmentHealthAttribute
  ( EnvironmentHealthAttribute
      ( ..,
        EnvironmentHealthAttribute_All,
        EnvironmentHealthAttribute_ApplicationMetrics,
        EnvironmentHealthAttribute_Causes,
        EnvironmentHealthAttribute_Color,
        EnvironmentHealthAttribute_HealthStatus,
        EnvironmentHealthAttribute_InstancesHealth,
        EnvironmentHealthAttribute_RefreshedAt,
        EnvironmentHealthAttribute_Status
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype EnvironmentHealthAttribute = EnvironmentHealthAttribute'
  { fromEnvironmentHealthAttribute ::
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

pattern EnvironmentHealthAttribute_All :: EnvironmentHealthAttribute
pattern EnvironmentHealthAttribute_All = EnvironmentHealthAttribute' "All"

pattern EnvironmentHealthAttribute_ApplicationMetrics :: EnvironmentHealthAttribute
pattern EnvironmentHealthAttribute_ApplicationMetrics = EnvironmentHealthAttribute' "ApplicationMetrics"

pattern EnvironmentHealthAttribute_Causes :: EnvironmentHealthAttribute
pattern EnvironmentHealthAttribute_Causes = EnvironmentHealthAttribute' "Causes"

pattern EnvironmentHealthAttribute_Color :: EnvironmentHealthAttribute
pattern EnvironmentHealthAttribute_Color = EnvironmentHealthAttribute' "Color"

pattern EnvironmentHealthAttribute_HealthStatus :: EnvironmentHealthAttribute
pattern EnvironmentHealthAttribute_HealthStatus = EnvironmentHealthAttribute' "HealthStatus"

pattern EnvironmentHealthAttribute_InstancesHealth :: EnvironmentHealthAttribute
pattern EnvironmentHealthAttribute_InstancesHealth = EnvironmentHealthAttribute' "InstancesHealth"

pattern EnvironmentHealthAttribute_RefreshedAt :: EnvironmentHealthAttribute
pattern EnvironmentHealthAttribute_RefreshedAt = EnvironmentHealthAttribute' "RefreshedAt"

pattern EnvironmentHealthAttribute_Status :: EnvironmentHealthAttribute
pattern EnvironmentHealthAttribute_Status = EnvironmentHealthAttribute' "Status"

{-# COMPLETE
  EnvironmentHealthAttribute_All,
  EnvironmentHealthAttribute_ApplicationMetrics,
  EnvironmentHealthAttribute_Causes,
  EnvironmentHealthAttribute_Color,
  EnvironmentHealthAttribute_HealthStatus,
  EnvironmentHealthAttribute_InstancesHealth,
  EnvironmentHealthAttribute_RefreshedAt,
  EnvironmentHealthAttribute_Status,
  EnvironmentHealthAttribute'
  #-}
