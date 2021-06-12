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
-- Module      : Network.AWS.EC2.Types.ClientVpnAuthenticationType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ClientVpnAuthenticationType
  ( ClientVpnAuthenticationType
      ( ..,
        ClientVpnAuthenticationType_Certificate_authentication,
        ClientVpnAuthenticationType_Directory_service_authentication,
        ClientVpnAuthenticationType_Federated_authentication
      ),
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal

newtype ClientVpnAuthenticationType = ClientVpnAuthenticationType'
  { fromClientVpnAuthenticationType ::
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

pattern ClientVpnAuthenticationType_Certificate_authentication :: ClientVpnAuthenticationType
pattern ClientVpnAuthenticationType_Certificate_authentication = ClientVpnAuthenticationType' "certificate-authentication"

pattern ClientVpnAuthenticationType_Directory_service_authentication :: ClientVpnAuthenticationType
pattern ClientVpnAuthenticationType_Directory_service_authentication = ClientVpnAuthenticationType' "directory-service-authentication"

pattern ClientVpnAuthenticationType_Federated_authentication :: ClientVpnAuthenticationType
pattern ClientVpnAuthenticationType_Federated_authentication = ClientVpnAuthenticationType' "federated-authentication"

{-# COMPLETE
  ClientVpnAuthenticationType_Certificate_authentication,
  ClientVpnAuthenticationType_Directory_service_authentication,
  ClientVpnAuthenticationType_Federated_authentication,
  ClientVpnAuthenticationType'
  #-}
