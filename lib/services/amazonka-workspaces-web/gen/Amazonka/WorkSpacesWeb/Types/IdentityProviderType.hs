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
-- Module      : Amazonka.WorkSpacesWeb.Types.IdentityProviderType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkSpacesWeb.Types.IdentityProviderType
  ( IdentityProviderType
      ( ..,
        IdentityProviderType_Facebook,
        IdentityProviderType_Google,
        IdentityProviderType_LoginWithAmazon,
        IdentityProviderType_OIDC,
        IdentityProviderType_SAML,
        IdentityProviderType_SignInWithApple
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

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

pattern IdentityProviderType_Facebook :: IdentityProviderType
pattern IdentityProviderType_Facebook = IdentityProviderType' "Facebook"

pattern IdentityProviderType_Google :: IdentityProviderType
pattern IdentityProviderType_Google = IdentityProviderType' "Google"

pattern IdentityProviderType_LoginWithAmazon :: IdentityProviderType
pattern IdentityProviderType_LoginWithAmazon = IdentityProviderType' "LoginWithAmazon"

pattern IdentityProviderType_OIDC :: IdentityProviderType
pattern IdentityProviderType_OIDC = IdentityProviderType' "OIDC"

pattern IdentityProviderType_SAML :: IdentityProviderType
pattern IdentityProviderType_SAML = IdentityProviderType' "SAML"

pattern IdentityProviderType_SignInWithApple :: IdentityProviderType
pattern IdentityProviderType_SignInWithApple = IdentityProviderType' "SignInWithApple"

{-# COMPLETE
  IdentityProviderType_Facebook,
  IdentityProviderType_Google,
  IdentityProviderType_LoginWithAmazon,
  IdentityProviderType_OIDC,
  IdentityProviderType_SAML,
  IdentityProviderType_SignInWithApple,
  IdentityProviderType'
  #-}
