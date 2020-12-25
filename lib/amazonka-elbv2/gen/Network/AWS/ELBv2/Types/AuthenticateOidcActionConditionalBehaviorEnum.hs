{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.Types.AuthenticateOidcActionConditionalBehaviorEnum
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELBv2.Types.AuthenticateOidcActionConditionalBehaviorEnum
  ( AuthenticateOidcActionConditionalBehaviorEnum
      ( AuthenticateOidcActionConditionalBehaviorEnum',
        AuthenticateOidcActionConditionalBehaviorEnumDeny,
        AuthenticateOidcActionConditionalBehaviorEnumAllow,
        AuthenticateOidcActionConditionalBehaviorEnumAuthenticate,
        fromAuthenticateOidcActionConditionalBehaviorEnum
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype AuthenticateOidcActionConditionalBehaviorEnum = AuthenticateOidcActionConditionalBehaviorEnum'
  { fromAuthenticateOidcActionConditionalBehaviorEnum ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern AuthenticateOidcActionConditionalBehaviorEnumDeny :: AuthenticateOidcActionConditionalBehaviorEnum
pattern AuthenticateOidcActionConditionalBehaviorEnumDeny = AuthenticateOidcActionConditionalBehaviorEnum' "deny"

pattern AuthenticateOidcActionConditionalBehaviorEnumAllow :: AuthenticateOidcActionConditionalBehaviorEnum
pattern AuthenticateOidcActionConditionalBehaviorEnumAllow = AuthenticateOidcActionConditionalBehaviorEnum' "allow"

pattern AuthenticateOidcActionConditionalBehaviorEnumAuthenticate :: AuthenticateOidcActionConditionalBehaviorEnum
pattern AuthenticateOidcActionConditionalBehaviorEnumAuthenticate = AuthenticateOidcActionConditionalBehaviorEnum' "authenticate"

{-# COMPLETE
  AuthenticateOidcActionConditionalBehaviorEnumDeny,
  AuthenticateOidcActionConditionalBehaviorEnumAllow,
  AuthenticateOidcActionConditionalBehaviorEnumAuthenticate,
  AuthenticateOidcActionConditionalBehaviorEnum'
  #-}
