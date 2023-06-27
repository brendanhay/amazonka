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
-- Module      : Amazonka.Glue.Types.JDBCConnectionType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.JDBCConnectionType
  ( JDBCConnectionType
      ( ..,
        JDBCConnectionType_Mysql,
        JDBCConnectionType_Oracle,
        JDBCConnectionType_Postgresql,
        JDBCConnectionType_Redshift,
        JDBCConnectionType_Sqlserver
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype JDBCConnectionType = JDBCConnectionType'
  { fromJDBCConnectionType ::
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

pattern JDBCConnectionType_Mysql :: JDBCConnectionType
pattern JDBCConnectionType_Mysql = JDBCConnectionType' "mysql"

pattern JDBCConnectionType_Oracle :: JDBCConnectionType
pattern JDBCConnectionType_Oracle = JDBCConnectionType' "oracle"

pattern JDBCConnectionType_Postgresql :: JDBCConnectionType
pattern JDBCConnectionType_Postgresql = JDBCConnectionType' "postgresql"

pattern JDBCConnectionType_Redshift :: JDBCConnectionType
pattern JDBCConnectionType_Redshift = JDBCConnectionType' "redshift"

pattern JDBCConnectionType_Sqlserver :: JDBCConnectionType
pattern JDBCConnectionType_Sqlserver = JDBCConnectionType' "sqlserver"

{-# COMPLETE
  JDBCConnectionType_Mysql,
  JDBCConnectionType_Oracle,
  JDBCConnectionType_Postgresql,
  JDBCConnectionType_Redshift,
  JDBCConnectionType_Sqlserver,
  JDBCConnectionType'
  #-}
