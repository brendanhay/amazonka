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
-- Module      : Amazonka.DMS.Types.KafkaSecurityProtocol
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DMS.Types.KafkaSecurityProtocol
  ( KafkaSecurityProtocol
      ( ..,
        KafkaSecurityProtocol_Plaintext,
        KafkaSecurityProtocol_Sasl_ssl,
        KafkaSecurityProtocol_Ssl_authentication,
        KafkaSecurityProtocol_Ssl_encryption
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype KafkaSecurityProtocol = KafkaSecurityProtocol'
  { fromKafkaSecurityProtocol ::
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

pattern KafkaSecurityProtocol_Plaintext :: KafkaSecurityProtocol
pattern KafkaSecurityProtocol_Plaintext = KafkaSecurityProtocol' "plaintext"

pattern KafkaSecurityProtocol_Sasl_ssl :: KafkaSecurityProtocol
pattern KafkaSecurityProtocol_Sasl_ssl = KafkaSecurityProtocol' "sasl-ssl"

pattern KafkaSecurityProtocol_Ssl_authentication :: KafkaSecurityProtocol
pattern KafkaSecurityProtocol_Ssl_authentication = KafkaSecurityProtocol' "ssl-authentication"

pattern KafkaSecurityProtocol_Ssl_encryption :: KafkaSecurityProtocol
pattern KafkaSecurityProtocol_Ssl_encryption = KafkaSecurityProtocol' "ssl-encryption"

{-# COMPLETE
  KafkaSecurityProtocol_Plaintext,
  KafkaSecurityProtocol_Sasl_ssl,
  KafkaSecurityProtocol_Ssl_authentication,
  KafkaSecurityProtocol_Ssl_encryption,
  KafkaSecurityProtocol'
  #-}
