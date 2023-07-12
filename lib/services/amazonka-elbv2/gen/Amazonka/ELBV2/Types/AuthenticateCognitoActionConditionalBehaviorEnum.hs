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
-- Module      : Amazonka.ELBV2.Types.AuthenticateCognitoActionConditionalBehaviorEnum
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ELBV2.Types.AuthenticateCognitoActionConditionalBehaviorEnum
  ( AuthenticateCognitoActionConditionalBehaviorEnum
      ( ..,
        AuthenticateCognitoActionConditionalBehaviorEnum_Allow,
        AuthenticateCognitoActionConditionalBehaviorEnum_Authenticate,
        AuthenticateCognitoActionConditionalBehaviorEnum_Deny
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype AuthenticateCognitoActionConditionalBehaviorEnum = AuthenticateCognitoActionConditionalBehaviorEnum'
  { fromAuthenticateCognitoActionConditionalBehaviorEnum ::
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
