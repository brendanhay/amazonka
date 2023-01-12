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
-- Module      : Amazonka.Lambda.Types.SourceAccessType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lambda.Types.SourceAccessType
  ( SourceAccessType
      ( ..,
        SourceAccessType_BASIC_AUTH,
        SourceAccessType_CLIENT_CERTIFICATE_TLS_AUTH,
        SourceAccessType_SASL_SCRAM_256_AUTH,
        SourceAccessType_SASL_SCRAM_512_AUTH,
        SourceAccessType_SERVER_ROOT_CA_CERTIFICATE,
        SourceAccessType_VIRTUAL_HOST,
        SourceAccessType_VPC_SECURITY_GROUP,
        SourceAccessType_VPC_SUBNET
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype SourceAccessType = SourceAccessType'
  { fromSourceAccessType ::
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

pattern SourceAccessType_BASIC_AUTH :: SourceAccessType
pattern SourceAccessType_BASIC_AUTH = SourceAccessType' "BASIC_AUTH"

pattern SourceAccessType_CLIENT_CERTIFICATE_TLS_AUTH :: SourceAccessType
pattern SourceAccessType_CLIENT_CERTIFICATE_TLS_AUTH = SourceAccessType' "CLIENT_CERTIFICATE_TLS_AUTH"

pattern SourceAccessType_SASL_SCRAM_256_AUTH :: SourceAccessType
pattern SourceAccessType_SASL_SCRAM_256_AUTH = SourceAccessType' "SASL_SCRAM_256_AUTH"

pattern SourceAccessType_SASL_SCRAM_512_AUTH :: SourceAccessType
pattern SourceAccessType_SASL_SCRAM_512_AUTH = SourceAccessType' "SASL_SCRAM_512_AUTH"

pattern SourceAccessType_SERVER_ROOT_CA_CERTIFICATE :: SourceAccessType
pattern SourceAccessType_SERVER_ROOT_CA_CERTIFICATE = SourceAccessType' "SERVER_ROOT_CA_CERTIFICATE"

pattern SourceAccessType_VIRTUAL_HOST :: SourceAccessType
pattern SourceAccessType_VIRTUAL_HOST = SourceAccessType' "VIRTUAL_HOST"

pattern SourceAccessType_VPC_SECURITY_GROUP :: SourceAccessType
pattern SourceAccessType_VPC_SECURITY_GROUP = SourceAccessType' "VPC_SECURITY_GROUP"

pattern SourceAccessType_VPC_SUBNET :: SourceAccessType
pattern SourceAccessType_VPC_SUBNET = SourceAccessType' "VPC_SUBNET"

{-# COMPLETE
  SourceAccessType_BASIC_AUTH,
  SourceAccessType_CLIENT_CERTIFICATE_TLS_AUTH,
  SourceAccessType_SASL_SCRAM_256_AUTH,
  SourceAccessType_SASL_SCRAM_512_AUTH,
  SourceAccessType_SERVER_ROOT_CA_CERTIFICATE,
  SourceAccessType_VIRTUAL_HOST,
  SourceAccessType_VPC_SECURITY_GROUP,
  SourceAccessType_VPC_SUBNET,
  SourceAccessType'
  #-}
