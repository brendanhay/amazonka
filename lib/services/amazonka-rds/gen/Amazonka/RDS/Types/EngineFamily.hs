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
-- Module      : Amazonka.RDS.Types.EngineFamily
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RDS.Types.EngineFamily
  ( EngineFamily
      ( ..,
        EngineFamily_MYSQL,
        EngineFamily_POSTGRESQL,
        EngineFamily_SQLSERVER
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype EngineFamily = EngineFamily'
  { fromEngineFamily ::
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

pattern EngineFamily_MYSQL :: EngineFamily
pattern EngineFamily_MYSQL = EngineFamily' "MYSQL"

pattern EngineFamily_POSTGRESQL :: EngineFamily
pattern EngineFamily_POSTGRESQL = EngineFamily' "POSTGRESQL"

pattern EngineFamily_SQLSERVER :: EngineFamily
pattern EngineFamily_SQLSERVER = EngineFamily' "SQLSERVER"

{-# COMPLETE
  EngineFamily_MYSQL,
  EngineFamily_POSTGRESQL,
  EngineFamily_SQLSERVER,
  EngineFamily'
  #-}
