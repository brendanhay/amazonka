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
-- Module      : Network.AWS.EC2.Types.ClientVpnAuthorizationRuleStatusCode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ClientVpnAuthorizationRuleStatusCode
  ( ClientVpnAuthorizationRuleStatusCode
      ( ..,
        ClientVpnAuthorizationRuleStatusCode_Active,
        ClientVpnAuthorizationRuleStatusCode_Authorizing,
        ClientVpnAuthorizationRuleStatusCode_Failed,
        ClientVpnAuthorizationRuleStatusCode_Revoking
      ),
  )
where

import Network.AWS.EC2.Internal
import qualified Network.AWS.Prelude as Prelude

newtype ClientVpnAuthorizationRuleStatusCode = ClientVpnAuthorizationRuleStatusCode'
  { fromClientVpnAuthorizationRuleStatusCode ::
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
