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
-- Module      : Amazonka.ResilienceHub.Types.ResourceMappingType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ResilienceHub.Types.ResourceMappingType
  ( ResourceMappingType
      ( ..,
        ResourceMappingType_AppRegistryApp,
        ResourceMappingType_CfnStack,
        ResourceMappingType_Resource,
        ResourceMappingType_ResourceGroup,
        ResourceMappingType_Terraform
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ResourceMappingType = ResourceMappingType'
  { fromResourceMappingType ::
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

pattern ResourceMappingType_AppRegistryApp :: ResourceMappingType
pattern ResourceMappingType_AppRegistryApp = ResourceMappingType' "AppRegistryApp"

pattern ResourceMappingType_CfnStack :: ResourceMappingType
pattern ResourceMappingType_CfnStack = ResourceMappingType' "CfnStack"

pattern ResourceMappingType_Resource :: ResourceMappingType
pattern ResourceMappingType_Resource = ResourceMappingType' "Resource"

pattern ResourceMappingType_ResourceGroup :: ResourceMappingType
pattern ResourceMappingType_ResourceGroup = ResourceMappingType' "ResourceGroup"

pattern ResourceMappingType_Terraform :: ResourceMappingType
pattern ResourceMappingType_Terraform = ResourceMappingType' "Terraform"

{-# COMPLETE
  ResourceMappingType_AppRegistryApp,
  ResourceMappingType_CfnStack,
  ResourceMappingType_Resource,
  ResourceMappingType_ResourceGroup,
  ResourceMappingType_Terraform,
  ResourceMappingType'
  #-}
