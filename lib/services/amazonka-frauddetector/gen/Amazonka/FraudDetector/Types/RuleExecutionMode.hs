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
-- Module      : Amazonka.FraudDetector.Types.RuleExecutionMode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FraudDetector.Types.RuleExecutionMode
  ( RuleExecutionMode
      ( ..,
        RuleExecutionMode_ALL_MATCHED,
        RuleExecutionMode_FIRST_MATCHED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype RuleExecutionMode = RuleExecutionMode'
  { fromRuleExecutionMode ::
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

pattern RuleExecutionMode_ALL_MATCHED :: RuleExecutionMode
pattern RuleExecutionMode_ALL_MATCHED = RuleExecutionMode' "ALL_MATCHED"

pattern RuleExecutionMode_FIRST_MATCHED :: RuleExecutionMode
pattern RuleExecutionMode_FIRST_MATCHED = RuleExecutionMode' "FIRST_MATCHED"

{-# COMPLETE
  RuleExecutionMode_ALL_MATCHED,
  RuleExecutionMode_FIRST_MATCHED,
  RuleExecutionMode'
  #-}
