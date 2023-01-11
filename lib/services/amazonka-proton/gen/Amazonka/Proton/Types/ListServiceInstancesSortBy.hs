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
-- Module      : Amazonka.Proton.Types.ListServiceInstancesSortBy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Proton.Types.ListServiceInstancesSortBy
  ( ListServiceInstancesSortBy
      ( ..,
        ListServiceInstancesSortBy_CreatedAt,
        ListServiceInstancesSortBy_DeploymentStatus,
        ListServiceInstancesSortBy_EnvironmentName,
        ListServiceInstancesSortBy_LastDeploymentAttemptedAt,
        ListServiceInstancesSortBy_Name,
        ListServiceInstancesSortBy_ServiceName,
        ListServiceInstancesSortBy_TemplateName
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ListServiceInstancesSortBy = ListServiceInstancesSortBy'
  { fromListServiceInstancesSortBy ::
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

pattern ListServiceInstancesSortBy_CreatedAt :: ListServiceInstancesSortBy
pattern ListServiceInstancesSortBy_CreatedAt = ListServiceInstancesSortBy' "createdAt"

pattern ListServiceInstancesSortBy_DeploymentStatus :: ListServiceInstancesSortBy
pattern ListServiceInstancesSortBy_DeploymentStatus = ListServiceInstancesSortBy' "deploymentStatus"

pattern ListServiceInstancesSortBy_EnvironmentName :: ListServiceInstancesSortBy
pattern ListServiceInstancesSortBy_EnvironmentName = ListServiceInstancesSortBy' "environmentName"

pattern ListServiceInstancesSortBy_LastDeploymentAttemptedAt :: ListServiceInstancesSortBy
pattern ListServiceInstancesSortBy_LastDeploymentAttemptedAt = ListServiceInstancesSortBy' "lastDeploymentAttemptedAt"

pattern ListServiceInstancesSortBy_Name :: ListServiceInstancesSortBy
pattern ListServiceInstancesSortBy_Name = ListServiceInstancesSortBy' "name"

pattern ListServiceInstancesSortBy_ServiceName :: ListServiceInstancesSortBy
pattern ListServiceInstancesSortBy_ServiceName = ListServiceInstancesSortBy' "serviceName"

pattern ListServiceInstancesSortBy_TemplateName :: ListServiceInstancesSortBy
pattern ListServiceInstancesSortBy_TemplateName = ListServiceInstancesSortBy' "templateName"

{-# COMPLETE
  ListServiceInstancesSortBy_CreatedAt,
  ListServiceInstancesSortBy_DeploymentStatus,
  ListServiceInstancesSortBy_EnvironmentName,
  ListServiceInstancesSortBy_LastDeploymentAttemptedAt,
  ListServiceInstancesSortBy_Name,
  ListServiceInstancesSortBy_ServiceName,
  ListServiceInstancesSortBy_TemplateName,
  ListServiceInstancesSortBy'
  #-}
