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
-- Module      : Amazonka.CognitoIdentityProvider.Types.IdentityProviderTypeType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CognitoIdentityProvider.Types.IdentityProviderTypeType
  ( IdentityProviderTypeType
      ( ..,
        IdentityProviderTypeType_Facebook,
        IdentityProviderTypeType_Google,
        IdentityProviderTypeType_LoginWithAmazon,
        IdentityProviderTypeType_OIDC,
        IdentityProviderTypeType_SAML,
        IdentityProviderTypeType_SignInWithApple
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype IdentityProviderTypeType = IdentityProviderTypeType'
  { fromIdentityProviderTypeType ::
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

pattern IdentityProviderTypeType_Facebook :: IdentityProviderTypeType
pattern IdentityProviderTypeType_Facebook = IdentityProviderTypeType' "Facebook"

pattern IdentityProviderTypeType_Google :: IdentityProviderTypeType
pattern IdentityProviderTypeType_Google = IdentityProviderTypeType' "Google"

pattern IdentityProviderTypeType_LoginWithAmazon :: IdentityProviderTypeType
pattern IdentityProviderTypeType_LoginWithAmazon = IdentityProviderTypeType' "LoginWithAmazon"

pattern IdentityProviderTypeType_OIDC :: IdentityProviderTypeType
pattern IdentityProviderTypeType_OIDC = IdentityProviderTypeType' "OIDC"

pattern IdentityProviderTypeType_SAML :: IdentityProviderTypeType
pattern IdentityProviderTypeType_SAML = IdentityProviderTypeType' "SAML"

pattern IdentityProviderTypeType_SignInWithApple :: IdentityProviderTypeType
pattern IdentityProviderTypeType_SignInWithApple = IdentityProviderTypeType' "SignInWithApple"

{-# COMPLETE
  IdentityProviderTypeType_Facebook,
  IdentityProviderTypeType_Google,
  IdentityProviderTypeType_LoginWithAmazon,
  IdentityProviderTypeType_OIDC,
  IdentityProviderTypeType_SAML,
  IdentityProviderTypeType_SignInWithApple,
  IdentityProviderTypeType'
  #-}
