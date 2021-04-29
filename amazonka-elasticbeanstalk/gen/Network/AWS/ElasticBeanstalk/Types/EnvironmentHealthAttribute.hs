{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.EnvironmentHealthAttribute
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.EnvironmentHealthAttribute
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

import qualified Network.AWS.Prelude as Prelude

newtype EnvironmentHealthAttribute = EnvironmentHealthAttribute'
  { fromEnvironmentHealthAttribute ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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
