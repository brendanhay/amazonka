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
-- Module      : Amazonka.ELBV2.Types.AuthenticateOidcActionConditionalBehaviorEnum
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ELBV2.Types.AuthenticateOidcActionConditionalBehaviorEnum
  ( AuthenticateOidcActionConditionalBehaviorEnum
      ( ..,
        AuthenticateOidcActionConditionalBehaviorEnum_Allow,
        AuthenticateOidcActionConditionalBehaviorEnum_Authenticate,
        AuthenticateOidcActionConditionalBehaviorEnum_Deny
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype AuthenticateOidcActionConditionalBehaviorEnum = AuthenticateOidcActionConditionalBehaviorEnum'
  { fromAuthenticateOidcActionConditionalBehaviorEnum ::
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
