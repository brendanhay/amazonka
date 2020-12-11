-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.IdentityProviderTypeType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.IdentityProviderTypeType
  ( IdentityProviderTypeType
      ( IdentityProviderTypeType',
        Facebook,
        Google,
        LoginWithAmazon,
        Oidc,
        Saml,
        SignInWithApple
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype IdentityProviderTypeType = IdentityProviderTypeType' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern Facebook :: IdentityProviderTypeType
pattern Facebook = IdentityProviderTypeType' "Facebook"

pattern Google :: IdentityProviderTypeType
pattern Google = IdentityProviderTypeType' "Google"

pattern LoginWithAmazon :: IdentityProviderTypeType
pattern LoginWithAmazon = IdentityProviderTypeType' "LoginWithAmazon"

pattern Oidc :: IdentityProviderTypeType
pattern Oidc = IdentityProviderTypeType' "OIDC"

pattern Saml :: IdentityProviderTypeType
pattern Saml = IdentityProviderTypeType' "SAML"

pattern SignInWithApple :: IdentityProviderTypeType
pattern SignInWithApple = IdentityProviderTypeType' "SignInWithApple"

{-# COMPLETE
  Facebook,
  Google,
  LoginWithAmazon,
  Oidc,
  Saml,
  SignInWithApple,
  IdentityProviderTypeType'
  #-}
