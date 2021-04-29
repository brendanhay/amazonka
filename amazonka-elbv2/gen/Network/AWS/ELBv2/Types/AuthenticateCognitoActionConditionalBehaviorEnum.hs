{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.Types.AuthenticateCognitoActionConditionalBehaviorEnum
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELBv2.Types.AuthenticateCognitoActionConditionalBehaviorEnum
  ( AuthenticateCognitoActionConditionalBehaviorEnum
      ( ..,
        AuthenticateCognitoActionConditionalBehaviorEnum_Allow,
        AuthenticateCognitoActionConditionalBehaviorEnum_Authenticate,
        AuthenticateCognitoActionConditionalBehaviorEnum_Deny
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype AuthenticateCognitoActionConditionalBehaviorEnum = AuthenticateCognitoActionConditionalBehaviorEnum'
  { fromAuthenticateCognitoActionConditionalBehaviorEnum ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
    )

pattern AuthenticateCognitoActionConditionalBehaviorEnum_Allow :: AuthenticateCognitoActionConditionalBehaviorEnum
pattern AuthenticateCognitoActionConditionalBehaviorEnum_Allow = AuthenticateCognitoActionConditionalBehaviorEnum' "allow"

pattern AuthenticateCognitoActionConditionalBehaviorEnum_Authenticate :: AuthenticateCognitoActionConditionalBehaviorEnum
pattern AuthenticateCognitoActionConditionalBehaviorEnum_Authenticate = AuthenticateCognitoActionConditionalBehaviorEnum' "authenticate"

pattern AuthenticateCognitoActionConditionalBehaviorEnum_Deny :: AuthenticateCognitoActionConditionalBehaviorEnum
pattern AuthenticateCognitoActionConditionalBehaviorEnum_Deny = AuthenticateCognitoActionConditionalBehaviorEnum' "deny"

{-# COMPLETE
  AuthenticateCognitoActionConditionalBehaviorEnum_Allow,
  AuthenticateCognitoActionConditionalBehaviorEnum_Authenticate,
  AuthenticateCognitoActionConditionalBehaviorEnum_Deny,
  AuthenticateCognitoActionConditionalBehaviorEnum'
  #-}
