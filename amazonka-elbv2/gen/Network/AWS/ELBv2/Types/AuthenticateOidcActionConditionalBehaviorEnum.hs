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
-- Module      : Network.AWS.ELBv2.Types.AuthenticateOidcActionConditionalBehaviorEnum
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELBv2.Types.AuthenticateOidcActionConditionalBehaviorEnum
  ( AuthenticateOidcActionConditionalBehaviorEnum
      ( ..,
        AuthenticateOidcActionConditionalBehaviorEnum_Allow,
        AuthenticateOidcActionConditionalBehaviorEnum_Authenticate,
        AuthenticateOidcActionConditionalBehaviorEnum_Deny
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype AuthenticateOidcActionConditionalBehaviorEnum = AuthenticateOidcActionConditionalBehaviorEnum'
  { fromAuthenticateOidcActionConditionalBehaviorEnum ::
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

pattern AuthenticateOidcActionConditionalBehaviorEnum_Allow :: AuthenticateOidcActionConditionalBehaviorEnum
pattern AuthenticateOidcActionConditionalBehaviorEnum_Allow = AuthenticateOidcActionConditionalBehaviorEnum' "allow"

pattern AuthenticateOidcActionConditionalBehaviorEnum_Authenticate :: AuthenticateOidcActionConditionalBehaviorEnum
pattern AuthenticateOidcActionConditionalBehaviorEnum_Authenticate = AuthenticateOidcActionConditionalBehaviorEnum' "authenticate"

pattern AuthenticateOidcActionConditionalBehaviorEnum_Deny :: AuthenticateOidcActionConditionalBehaviorEnum
pattern AuthenticateOidcActionConditionalBehaviorEnum_Deny = AuthenticateOidcActionConditionalBehaviorEnum' "deny"

{-# COMPLETE
  AuthenticateOidcActionConditionalBehaviorEnum_Allow,
  AuthenticateOidcActionConditionalBehaviorEnum_Authenticate,
  AuthenticateOidcActionConditionalBehaviorEnum_Deny,
  AuthenticateOidcActionConditionalBehaviorEnum'
  #-}
