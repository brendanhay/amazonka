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
-- Maintainer  : Brendan Hay
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

-- | Returns information related to the type of user authentication that is
-- in use for a file transfer protocol-enabled server\'s users. For
-- @AWS_DIRECTORY_SERVICE@ or @SERVICE_MANAGED@ authentication, the Secure
-- Shell (SSH) public keys are stored with a user on the server instance.
-- For @API_GATEWAY@ authentication, your custom authentication method is
-- implemented by using an API call. The server can have only one method of
-- authentication.
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
