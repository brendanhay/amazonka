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
-- Module      : Amazonka.EC2.Types.ClientVpnAuthorizationRuleStatusCode
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.ClientVpnAuthorizationRuleStatusCode
  ( ClientVpnAuthorizationRuleStatusCode
      ( ..,
        ClientVpnAuthorizationRuleStatusCode_Active,
        ClientVpnAuthorizationRuleStatusCode_Authorizing,
        ClientVpnAuthorizationRuleStatusCode_Failed,
        ClientVpnAuthorizationRuleStatusCode_Revoking
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

newtype ClientVpnAuthorizationRuleStatusCode = ClientVpnAuthorizationRuleStatusCode'
  { fromClientVpnAuthorizationRuleStatusCode ::
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

pattern ClientVpnAuthorizationRuleStatusCode_Active :: ClientVpnAuthorizationRuleStatusCode
pattern ClientVpnAuthorizationRuleStatusCode_Active = ClientVpnAuthorizationRuleStatusCode' "active"

pattern ClientVpnAuthorizationRuleStatusCode_Authorizing :: ClientVpnAuthorizationRuleStatusCode
pattern ClientVpnAuthorizationRuleStatusCode_Authorizing = ClientVpnAuthorizationRuleStatusCode' "authorizing"

pattern ClientVpnAuthorizationRuleStatusCode_Failed :: ClientVpnAuthorizationRuleStatusCode
pattern ClientVpnAuthorizationRuleStatusCode_Failed = ClientVpnAuthorizationRuleStatusCode' "failed"

pattern ClientVpnAuthorizationRuleStatusCode_Revoking :: ClientVpnAuthorizationRuleStatusCode
pattern ClientVpnAuthorizationRuleStatusCode_Revoking = ClientVpnAuthorizationRuleStatusCode' "revoking"

{-# COMPLETE
  ClientVpnAuthorizationRuleStatusCode_Active,
  ClientVpnAuthorizationRuleStatusCode_Authorizing,
  ClientVpnAuthorizationRuleStatusCode_Failed,
  ClientVpnAuthorizationRuleStatusCode_Revoking,
  ClientVpnAuthorizationRuleStatusCode'
  #-}
