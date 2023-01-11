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
-- Module      : Amazonka.RDS.Types.ClientPasswordAuthType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RDS.Types.ClientPasswordAuthType
  ( ClientPasswordAuthType
      ( ..,
        ClientPasswordAuthType_MYSQL_NATIVE_PASSWORD,
        ClientPasswordAuthType_POSTGRES_MD5,
        ClientPasswordAuthType_POSTGRES_SCRAM_SHA_256,
        ClientPasswordAuthType_SQL_SERVER_AUTHENTICATION
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ClientPasswordAuthType = ClientPasswordAuthType'
  { fromClientPasswordAuthType ::
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

pattern ClientPasswordAuthType_MYSQL_NATIVE_PASSWORD :: ClientPasswordAuthType
pattern ClientPasswordAuthType_MYSQL_NATIVE_PASSWORD = ClientPasswordAuthType' "MYSQL_NATIVE_PASSWORD"

pattern ClientPasswordAuthType_POSTGRES_MD5 :: ClientPasswordAuthType
pattern ClientPasswordAuthType_POSTGRES_MD5 = ClientPasswordAuthType' "POSTGRES_MD5"

pattern ClientPasswordAuthType_POSTGRES_SCRAM_SHA_256 :: ClientPasswordAuthType
pattern ClientPasswordAuthType_POSTGRES_SCRAM_SHA_256 = ClientPasswordAuthType' "POSTGRES_SCRAM_SHA_256"

pattern ClientPasswordAuthType_SQL_SERVER_AUTHENTICATION :: ClientPasswordAuthType
pattern ClientPasswordAuthType_SQL_SERVER_AUTHENTICATION = ClientPasswordAuthType' "SQL_SERVER_AUTHENTICATION"

{-# COMPLETE
  ClientPasswordAuthType_MYSQL_NATIVE_PASSWORD,
  ClientPasswordAuthType_POSTGRES_MD5,
  ClientPasswordAuthType_POSTGRES_SCRAM_SHA_256,
  ClientPasswordAuthType_SQL_SERVER_AUTHENTICATION,
  ClientPasswordAuthType'
  #-}
