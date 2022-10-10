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
-- Copyright   : (c) 2013-2022 Brendan Hay
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
import qualified Amazonka.Prelude as Prelude

newtype Operation = Operation'
  { fromOperation ::
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
