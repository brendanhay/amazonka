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
-- Module      : Network.AWS.Grafana.Types.AuthenticationProviderTypes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Grafana.Types.AuthenticationProviderTypes
  ( AuthenticationProviderTypes
      ( ..,
        AuthenticationProviderTypes_AWS_SSO,
        AuthenticationProviderTypes_SAML
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype AuthenticationProviderTypes = AuthenticationProviderTypes'
  { fromAuthenticationProviderTypes ::
      Core.Text
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
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
    )

pattern AuthenticationProviderTypes_AWS_SSO :: AuthenticationProviderTypes
pattern AuthenticationProviderTypes_AWS_SSO = AuthenticationProviderTypes' "AWS_SSO"

pattern AuthenticationProviderTypes_SAML :: AuthenticationProviderTypes
pattern AuthenticationProviderTypes_SAML = AuthenticationProviderTypes' "SAML"

{-# COMPLETE
  AuthenticationProviderTypes_AWS_SSO,
  AuthenticationProviderTypes_SAML,
  AuthenticationProviderTypes'
  #-}
