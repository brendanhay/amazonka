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
-- Module      : Amazonka.Kendra.Types.DatabaseEngineType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.DatabaseEngineType
  ( DatabaseEngineType
      ( ..,
        DatabaseEngineType_RDS_AURORA_MYSQL,
        DatabaseEngineType_RDS_AURORA_POSTGRESQL,
        DatabaseEngineType_RDS_MYSQL,
        DatabaseEngineType_RDS_POSTGRESQL
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype DatabaseEngineType = DatabaseEngineType'
  { fromDatabaseEngineType ::
      Core.Text
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

pattern DatabaseEngineType_RDS_AURORA_MYSQL :: DatabaseEngineType
pattern DatabaseEngineType_RDS_AURORA_MYSQL = DatabaseEngineType' "RDS_AURORA_MYSQL"

pattern DatabaseEngineType_RDS_AURORA_POSTGRESQL :: DatabaseEngineType
pattern DatabaseEngineType_RDS_AURORA_POSTGRESQL = DatabaseEngineType' "RDS_AURORA_POSTGRESQL"

pattern DatabaseEngineType_RDS_MYSQL :: DatabaseEngineType
pattern DatabaseEngineType_RDS_MYSQL = DatabaseEngineType' "RDS_MYSQL"

pattern DatabaseEngineType_RDS_POSTGRESQL :: DatabaseEngineType
pattern DatabaseEngineType_RDS_POSTGRESQL = DatabaseEngineType' "RDS_POSTGRESQL"

{-# COMPLETE
  DatabaseEngineType_RDS_AURORA_MYSQL,
  DatabaseEngineType_RDS_AURORA_POSTGRESQL,
  DatabaseEngineType_RDS_MYSQL,
  DatabaseEngineType_RDS_POSTGRESQL,
  DatabaseEngineType'
  #-}
