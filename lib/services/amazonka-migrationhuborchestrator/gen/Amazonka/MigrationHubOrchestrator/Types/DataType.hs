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
-- Module      : Amazonka.MigrationHubOrchestrator.Types.DataType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MigrationHubOrchestrator.Types.DataType
  ( DataType
      ( ..,
        DataType_INTEGER,
        DataType_STRING,
        DataType_STRINGLIST,
        DataType_STRINGMAP
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype DataType = DataType'
  { fromDataType ::
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

pattern DataType_INTEGER :: DataType
pattern DataType_INTEGER = DataType' "INTEGER"

pattern DataType_STRING :: DataType
pattern DataType_STRING = DataType' "STRING"

pattern DataType_STRINGLIST :: DataType
pattern DataType_STRINGLIST = DataType' "STRINGLIST"

pattern DataType_STRINGMAP :: DataType
pattern DataType_STRINGMAP = DataType' "STRINGMAP"

{-# COMPLETE
  DataType_INTEGER,
  DataType_STRING,
  DataType_STRINGLIST,
  DataType_STRINGMAP,
  DataType'
  #-}
