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
-- Module      : Amazonka.Forecast.Types.Operation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Forecast.Types.Operation
  ( Operation
      ( ..,
        Operation_ADD,
        Operation_DIVIDE,
        Operation_MULTIPLY,
        Operation_SUBTRACT
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype Operation = Operation'
  { fromOperation ::
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

pattern Operation_ADD :: Operation
pattern Operation_ADD = Operation' "ADD"

pattern Operation_DIVIDE :: Operation
pattern Operation_DIVIDE = Operation' "DIVIDE"

pattern Operation_MULTIPLY :: Operation
pattern Operation_MULTIPLY = Operation' "MULTIPLY"

pattern Operation_SUBTRACT :: Operation
pattern Operation_SUBTRACT = Operation' "SUBTRACT"

{-# COMPLETE
  Operation_ADD,
  Operation_DIVIDE,
  Operation_MULTIPLY,
  Operation_SUBTRACT,
  Operation'
  #-}
