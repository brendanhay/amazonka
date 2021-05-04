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

import Network.AWS.EC2.Internal
import qualified Network.AWS.Prelude as Prelude

newtype ClientVpnAuthenticationType = ClientVpnAuthenticationType'
  { fromClientVpnAuthenticationType ::
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
