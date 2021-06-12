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
-- Module      : Network.AWS.AppStream.Types.AuthenticationType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppStream.Types.AuthenticationType
  ( AuthenticationType
      ( ..,
        AuthenticationType_API,
        AuthenticationType_SAML,
        AuthenticationType_USERPOOL
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype AuthenticationType = AuthenticationType'
  { fromAuthenticationType ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
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

pattern AuthenticationType_API :: AuthenticationType
pattern AuthenticationType_API = AuthenticationType' "API"

pattern AuthenticationType_SAML :: AuthenticationType
pattern AuthenticationType_SAML = AuthenticationType' "SAML"

pattern AuthenticationType_USERPOOL :: AuthenticationType
pattern AuthenticationType_USERPOOL = AuthenticationType' "USERPOOL"

{-# COMPLETE
  AuthenticationType_API,
  AuthenticationType_SAML,
  AuthenticationType_USERPOOL,
  AuthenticationType'
  #-}
