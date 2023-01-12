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
-- Module      : Amazonka.CloudDirectory.Types.RuleType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudDirectory.Types.RuleType
  ( RuleType
      ( ..,
        RuleType_BINARY_LENGTH,
        RuleType_NUMBER_COMPARISON,
        RuleType_STRING_FROM_SET,
        RuleType_STRING_LENGTH
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype RuleType = RuleType'
  { fromRuleType ::
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

pattern RuleType_BINARY_LENGTH :: RuleType
pattern RuleType_BINARY_LENGTH = RuleType' "BINARY_LENGTH"

pattern RuleType_NUMBER_COMPARISON :: RuleType
pattern RuleType_NUMBER_COMPARISON = RuleType' "NUMBER_COMPARISON"

pattern RuleType_STRING_FROM_SET :: RuleType
pattern RuleType_STRING_FROM_SET = RuleType' "STRING_FROM_SET"

pattern RuleType_STRING_LENGTH :: RuleType
pattern RuleType_STRING_LENGTH = RuleType' "STRING_LENGTH"

{-# COMPLETE
  RuleType_BINARY_LENGTH,
  RuleType_NUMBER_COMPARISON,
  RuleType_STRING_FROM_SET,
  RuleType_STRING_LENGTH,
  RuleType'
  #-}
