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
-- Module      : Amazonka.Transfer.Types.IdentityProviderType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Transfer.Types.IdentityProviderType
  ( IdentityProviderType
      ( ..,
        IdentityProviderType_API_GATEWAY,
        IdentityProviderType_AWS_DIRECTORY_SERVICE,
        IdentityProviderType_AWS_LAMBDA,
        IdentityProviderType_SERVICE_MANAGED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The mode of authentication for a server. The default value is
-- @SERVICE_MANAGED@, which allows you to store and access user credentials
-- within the Transfer Family service.
--
-- Use @AWS_DIRECTORY_SERVICE@ to provide access to Active Directory groups
-- in Directory Service for Microsoft Active Directory or Microsoft Active
-- Directory in your on-premises environment or in Amazon Web Services
-- using AD Connector. This option also requires you to provide a Directory
-- ID by using the @IdentityProviderDetails@ parameter.
--
-- Use the @API_GATEWAY@ value to integrate with an identity provider of
-- your choosing. The @API_GATEWAY@ setting requires you to provide an
-- Amazon API Gateway endpoint URL to call for authentication by using the
-- @IdentityProviderDetails@ parameter.
--
-- Use the @AWS_LAMBDA@ value to directly use an Lambda function as your
-- identity provider. If you choose this value, you must specify the ARN
-- for the Lambda function in the @Function@ parameter for the
-- @IdentityProviderDetails@ data type.
newtype IdentityProviderType = IdentityProviderType'
  { fromIdentityProviderType ::
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

pattern IdentityProviderType_API_GATEWAY :: IdentityProviderType
pattern IdentityProviderType_API_GATEWAY = IdentityProviderType' "API_GATEWAY"

pattern IdentityProviderType_AWS_DIRECTORY_SERVICE :: IdentityProviderType
pattern IdentityProviderType_AWS_DIRECTORY_SERVICE = IdentityProviderType' "AWS_DIRECTORY_SERVICE"

pattern IdentityProviderType_AWS_LAMBDA :: IdentityProviderType
pattern IdentityProviderType_AWS_LAMBDA = IdentityProviderType' "AWS_LAMBDA"

pattern IdentityProviderType_SERVICE_MANAGED :: IdentityProviderType
pattern IdentityProviderType_SERVICE_MANAGED = IdentityProviderType' "SERVICE_MANAGED"

{-# COMPLETE
  IdentityProviderType_API_GATEWAY,
  IdentityProviderType_AWS_DIRECTORY_SERVICE,
  IdentityProviderType_AWS_LAMBDA,
  IdentityProviderType_SERVICE_MANAGED,
  IdentityProviderType'
  #-}
