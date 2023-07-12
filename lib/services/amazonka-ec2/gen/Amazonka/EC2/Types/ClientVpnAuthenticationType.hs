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
-- Module      : Amazonka.EC2.Types.ClientVpnAuthenticationType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.ClientVpnAuthenticationType
  ( ClientVpnAuthenticationType
      ( ..,
        ClientVpnAuthenticationType_Certificate_authentication,
        ClientVpnAuthenticationType_Directory_service_authentication,
        ClientVpnAuthenticationType_Federated_authentication
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

newtype ClientVpnAuthenticationType = ClientVpnAuthenticationType'
  { fromClientVpnAuthenticationType ::
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
