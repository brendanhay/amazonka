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
-- Module      : Amazonka.MigrationHubReFactorSpaces.Types.ErrorResourceType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MigrationHubReFactorSpaces.Types.ErrorResourceType
  ( ErrorResourceType
      ( ..,
        ErrorResourceType_API_GATEWAY,
        ErrorResourceType_APPLICATION,
        ErrorResourceType_ENVIRONMENT,
        ErrorResourceType_IAM_ROLE,
        ErrorResourceType_LAMBDA,
        ErrorResourceType_LOAD_BALANCER_LISTENER,
        ErrorResourceType_NLB,
        ErrorResourceType_RESOURCE_SHARE,
        ErrorResourceType_ROUTE,
        ErrorResourceType_ROUTE_TABLE,
        ErrorResourceType_SECURITY_GROUP,
        ErrorResourceType_SERVICE,
        ErrorResourceType_SUBNET,
        ErrorResourceType_TARGET_GROUP,
        ErrorResourceType_TRANSIT_GATEWAY,
        ErrorResourceType_TRANSIT_GATEWAY_ATTACHMENT,
        ErrorResourceType_VPC,
        ErrorResourceType_VPC_ENDPOINT_SERVICE_CONFIGURATION,
        ErrorResourceType_VPC_LINK
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ErrorResourceType = ErrorResourceType'
  { fromErrorResourceType ::
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

pattern ErrorResourceType_API_GATEWAY :: ErrorResourceType
pattern ErrorResourceType_API_GATEWAY = ErrorResourceType' "API_GATEWAY"

pattern ErrorResourceType_APPLICATION :: ErrorResourceType
pattern ErrorResourceType_APPLICATION = ErrorResourceType' "APPLICATION"

pattern ErrorResourceType_ENVIRONMENT :: ErrorResourceType
pattern ErrorResourceType_ENVIRONMENT = ErrorResourceType' "ENVIRONMENT"

pattern ErrorResourceType_IAM_ROLE :: ErrorResourceType
pattern ErrorResourceType_IAM_ROLE = ErrorResourceType' "IAM_ROLE"

pattern ErrorResourceType_LAMBDA :: ErrorResourceType
pattern ErrorResourceType_LAMBDA = ErrorResourceType' "LAMBDA"

pattern ErrorResourceType_LOAD_BALANCER_LISTENER :: ErrorResourceType
pattern ErrorResourceType_LOAD_BALANCER_LISTENER = ErrorResourceType' "LOAD_BALANCER_LISTENER"

pattern ErrorResourceType_NLB :: ErrorResourceType
pattern ErrorResourceType_NLB = ErrorResourceType' "NLB"

pattern ErrorResourceType_RESOURCE_SHARE :: ErrorResourceType
pattern ErrorResourceType_RESOURCE_SHARE = ErrorResourceType' "RESOURCE_SHARE"

pattern ErrorResourceType_ROUTE :: ErrorResourceType
pattern ErrorResourceType_ROUTE = ErrorResourceType' "ROUTE"

pattern ErrorResourceType_ROUTE_TABLE :: ErrorResourceType
pattern ErrorResourceType_ROUTE_TABLE = ErrorResourceType' "ROUTE_TABLE"

pattern ErrorResourceType_SECURITY_GROUP :: ErrorResourceType
pattern ErrorResourceType_SECURITY_GROUP = ErrorResourceType' "SECURITY_GROUP"

pattern ErrorResourceType_SERVICE :: ErrorResourceType
pattern ErrorResourceType_SERVICE = ErrorResourceType' "SERVICE"

pattern ErrorResourceType_SUBNET :: ErrorResourceType
pattern ErrorResourceType_SUBNET = ErrorResourceType' "SUBNET"

pattern ErrorResourceType_TARGET_GROUP :: ErrorResourceType
pattern ErrorResourceType_TARGET_GROUP = ErrorResourceType' "TARGET_GROUP"

pattern ErrorResourceType_TRANSIT_GATEWAY :: ErrorResourceType
pattern ErrorResourceType_TRANSIT_GATEWAY = ErrorResourceType' "TRANSIT_GATEWAY"

pattern ErrorResourceType_TRANSIT_GATEWAY_ATTACHMENT :: ErrorResourceType
pattern ErrorResourceType_TRANSIT_GATEWAY_ATTACHMENT = ErrorResourceType' "TRANSIT_GATEWAY_ATTACHMENT"

pattern ErrorResourceType_VPC :: ErrorResourceType
pattern ErrorResourceType_VPC = ErrorResourceType' "VPC"

pattern ErrorResourceType_VPC_ENDPOINT_SERVICE_CONFIGURATION :: ErrorResourceType
pattern ErrorResourceType_VPC_ENDPOINT_SERVICE_CONFIGURATION = ErrorResourceType' "VPC_ENDPOINT_SERVICE_CONFIGURATION"

pattern ErrorResourceType_VPC_LINK :: ErrorResourceType
pattern ErrorResourceType_VPC_LINK = ErrorResourceType' "VPC_LINK"

{-# COMPLETE
  ErrorResourceType_API_GATEWAY,
  ErrorResourceType_APPLICATION,
  ErrorResourceType_ENVIRONMENT,
  ErrorResourceType_IAM_ROLE,
  ErrorResourceType_LAMBDA,
  ErrorResourceType_LOAD_BALANCER_LISTENER,
  ErrorResourceType_NLB,
  ErrorResourceType_RESOURCE_SHARE,
  ErrorResourceType_ROUTE,
  ErrorResourceType_ROUTE_TABLE,
  ErrorResourceType_SECURITY_GROUP,
  ErrorResourceType_SERVICE,
  ErrorResourceType_SUBNET,
  ErrorResourceType_TARGET_GROUP,
  ErrorResourceType_TRANSIT_GATEWAY,
  ErrorResourceType_TRANSIT_GATEWAY_ATTACHMENT,
  ErrorResourceType_VPC,
  ErrorResourceType_VPC_ENDPOINT_SERVICE_CONFIGURATION,
  ErrorResourceType_VPC_LINK,
  ErrorResourceType'
  #-}
