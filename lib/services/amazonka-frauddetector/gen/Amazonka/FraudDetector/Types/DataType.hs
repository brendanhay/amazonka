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
-- Module      : Amazonka.FraudDetector.Types.DataType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FraudDetector.Types.DataType
  ( DataType
      ( ..,
        DataType_BOOLEAN,
        DataType_FLOAT,
        DataType_INTEGER,
        DataType_STRING
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

pattern DataType_BOOLEAN :: DataType
pattern DataType_BOOLEAN = DataType' "BOOLEAN"

pattern DataType_FLOAT :: DataType
pattern DataType_FLOAT = DataType' "FLOAT"

pattern DataType_INTEGER :: DataType
pattern DataType_INTEGER = DataType' "INTEGER"

pattern DataType_STRING :: DataType
pattern DataType_STRING = DataType' "STRING"

{-# COMPLETE
  DataType_BOOLEAN,
  DataType_FLOAT,
  DataType_INTEGER,
  DataType_STRING,
  DataType'
  #-}
