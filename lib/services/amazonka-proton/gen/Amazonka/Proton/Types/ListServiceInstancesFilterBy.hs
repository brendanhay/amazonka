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
-- Module      : Amazonka.Proton.Types.ListServiceInstancesFilterBy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Proton.Types.ListServiceInstancesFilterBy
  ( ListServiceInstancesFilterBy
      ( ..,
        ListServiceInstancesFilterBy_CreatedAtAfter,
        ListServiceInstancesFilterBy_CreatedAtBefore,
        ListServiceInstancesFilterBy_DeployedTemplateVersionStatus,
        ListServiceInstancesFilterBy_DeploymentStatus,
        ListServiceInstancesFilterBy_EnvironmentName,
        ListServiceInstancesFilterBy_LastDeploymentAttemptedAtAfter,
        ListServiceInstancesFilterBy_LastDeploymentAttemptedAtBefore,
        ListServiceInstancesFilterBy_Name,
        ListServiceInstancesFilterBy_ServiceName,
        ListServiceInstancesFilterBy_TemplateName
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ListServiceInstancesFilterBy = ListServiceInstancesFilterBy'
  { fromListServiceInstancesFilterBy ::
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

pattern ListServiceInstancesFilterBy_CreatedAtAfter :: ListServiceInstancesFilterBy
pattern ListServiceInstancesFilterBy_CreatedAtAfter = ListServiceInstancesFilterBy' "createdAtAfter"

pattern ListServiceInstancesFilterBy_CreatedAtBefore :: ListServiceInstancesFilterBy
pattern ListServiceInstancesFilterBy_CreatedAtBefore = ListServiceInstancesFilterBy' "createdAtBefore"

pattern ListServiceInstancesFilterBy_DeployedTemplateVersionStatus :: ListServiceInstancesFilterBy
pattern ListServiceInstancesFilterBy_DeployedTemplateVersionStatus = ListServiceInstancesFilterBy' "deployedTemplateVersionStatus"

pattern ListServiceInstancesFilterBy_DeploymentStatus :: ListServiceInstancesFilterBy
pattern ListServiceInstancesFilterBy_DeploymentStatus = ListServiceInstancesFilterBy' "deploymentStatus"

pattern ListServiceInstancesFilterBy_EnvironmentName :: ListServiceInstancesFilterBy
pattern ListServiceInstancesFilterBy_EnvironmentName = ListServiceInstancesFilterBy' "environmentName"

pattern ListServiceInstancesFilterBy_LastDeploymentAttemptedAtAfter :: ListServiceInstancesFilterBy
pattern ListServiceInstancesFilterBy_LastDeploymentAttemptedAtAfter = ListServiceInstancesFilterBy' "lastDeploymentAttemptedAtAfter"

pattern ListServiceInstancesFilterBy_LastDeploymentAttemptedAtBefore :: ListServiceInstancesFilterBy
pattern ListServiceInstancesFilterBy_LastDeploymentAttemptedAtBefore = ListServiceInstancesFilterBy' "lastDeploymentAttemptedAtBefore"

pattern ListServiceInstancesFilterBy_Name :: ListServiceInstancesFilterBy
pattern ListServiceInstancesFilterBy_Name = ListServiceInstancesFilterBy' "name"

pattern ListServiceInstancesFilterBy_ServiceName :: ListServiceInstancesFilterBy
pattern ListServiceInstancesFilterBy_ServiceName = ListServiceInstancesFilterBy' "serviceName"

pattern ListServiceInstancesFilterBy_TemplateName :: ListServiceInstancesFilterBy
pattern ListServiceInstancesFilterBy_TemplateName = ListServiceInstancesFilterBy' "templateName"

{-# COMPLETE
  ListServiceInstancesFilterBy_CreatedAtAfter,
  ListServiceInstancesFilterBy_CreatedAtBefore,
  ListServiceInstancesFilterBy_DeployedTemplateVersionStatus,
  ListServiceInstancesFilterBy_DeploymentStatus,
  ListServiceInstancesFilterBy_EnvironmentName,
  ListServiceInstancesFilterBy_LastDeploymentAttemptedAtAfter,
  ListServiceInstancesFilterBy_LastDeploymentAttemptedAtBefore,
  ListServiceInstancesFilterBy_Name,
  ListServiceInstancesFilterBy_ServiceName,
  ListServiceInstancesFilterBy_TemplateName,
  ListServiceInstancesFilterBy'
  #-}
