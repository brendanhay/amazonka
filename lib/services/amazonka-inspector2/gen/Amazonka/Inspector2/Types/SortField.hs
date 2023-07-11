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
-- Module      : Amazonka.Inspector2.Types.SortField
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector2.Types.SortField
  ( SortField
      ( ..,
        SortField_AWS_ACCOUNT_ID,
        SortField_COMPONENT_TYPE,
        SortField_ECR_IMAGE_PUSHED_AT,
        SortField_ECR_IMAGE_REGISTRY,
        SortField_ECR_IMAGE_REPOSITORY_NAME,
        SortField_FINDING_STATUS,
        SortField_FINDING_TYPE,
        SortField_FIRST_OBSERVED_AT,
        SortField_INSPECTOR_SCORE,
        SortField_LAST_OBSERVED_AT,
        SortField_NETWORK_PROTOCOL,
        SortField_RESOURCE_TYPE,
        SortField_SEVERITY,
        SortField_VENDOR_SEVERITY,
        SortField_VULNERABILITY_ID,
        SortField_VULNERABILITY_SOURCE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype SortField = SortField'
  { fromSortField ::
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

pattern SortField_AWS_ACCOUNT_ID :: SortField
pattern SortField_AWS_ACCOUNT_ID = SortField' "AWS_ACCOUNT_ID"

pattern SortField_COMPONENT_TYPE :: SortField
pattern SortField_COMPONENT_TYPE = SortField' "COMPONENT_TYPE"

pattern SortField_ECR_IMAGE_PUSHED_AT :: SortField
pattern SortField_ECR_IMAGE_PUSHED_AT = SortField' "ECR_IMAGE_PUSHED_AT"

pattern SortField_ECR_IMAGE_REGISTRY :: SortField
pattern SortField_ECR_IMAGE_REGISTRY = SortField' "ECR_IMAGE_REGISTRY"

pattern SortField_ECR_IMAGE_REPOSITORY_NAME :: SortField
pattern SortField_ECR_IMAGE_REPOSITORY_NAME = SortField' "ECR_IMAGE_REPOSITORY_NAME"

pattern SortField_FINDING_STATUS :: SortField
pattern SortField_FINDING_STATUS = SortField' "FINDING_STATUS"

pattern SortField_FINDING_TYPE :: SortField
pattern SortField_FINDING_TYPE = SortField' "FINDING_TYPE"

pattern SortField_FIRST_OBSERVED_AT :: SortField
pattern SortField_FIRST_OBSERVED_AT = SortField' "FIRST_OBSERVED_AT"

pattern SortField_INSPECTOR_SCORE :: SortField
pattern SortField_INSPECTOR_SCORE = SortField' "INSPECTOR_SCORE"

pattern SortField_LAST_OBSERVED_AT :: SortField
pattern SortField_LAST_OBSERVED_AT = SortField' "LAST_OBSERVED_AT"

pattern SortField_NETWORK_PROTOCOL :: SortField
pattern SortField_NETWORK_PROTOCOL = SortField' "NETWORK_PROTOCOL"

pattern SortField_RESOURCE_TYPE :: SortField
pattern SortField_RESOURCE_TYPE = SortField' "RESOURCE_TYPE"

pattern SortField_SEVERITY :: SortField
pattern SortField_SEVERITY = SortField' "SEVERITY"

pattern SortField_VENDOR_SEVERITY :: SortField
pattern SortField_VENDOR_SEVERITY = SortField' "VENDOR_SEVERITY"

pattern SortField_VULNERABILITY_ID :: SortField
pattern SortField_VULNERABILITY_ID = SortField' "VULNERABILITY_ID"

pattern SortField_VULNERABILITY_SOURCE :: SortField
pattern SortField_VULNERABILITY_SOURCE = SortField' "VULNERABILITY_SOURCE"

{-# COMPLETE
  SortField_AWS_ACCOUNT_ID,
  SortField_COMPONENT_TYPE,
  SortField_ECR_IMAGE_PUSHED_AT,
  SortField_ECR_IMAGE_REGISTRY,
  SortField_ECR_IMAGE_REPOSITORY_NAME,
  SortField_FINDING_STATUS,
  SortField_FINDING_TYPE,
  SortField_FIRST_OBSERVED_AT,
  SortField_INSPECTOR_SCORE,
  SortField_LAST_OBSERVED_AT,
  SortField_NETWORK_PROTOCOL,
  SortField_RESOURCE_TYPE,
  SortField_SEVERITY,
  SortField_VENDOR_SEVERITY,
  SortField_VULNERABILITY_ID,
  SortField_VULNERABILITY_SOURCE,
  SortField'
  #-}
