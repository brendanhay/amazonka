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
-- Module      : Amazonka.DataSync.Types.Operator
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataSync.Types.Operator
  ( Operator
      ( ..,
        Operator_BeginsWith,
        Operator_Contains,
        Operator_Equals,
        Operator_GreaterThan,
        Operator_GreaterThanOrEqual,
        Operator_In,
        Operator_LessThan,
        Operator_LessThanOrEqual,
        Operator_NotContains,
        Operator_NotEquals
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype Operator = Operator'
  { fromOperator ::
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

pattern Operator_BeginsWith :: Operator
pattern Operator_BeginsWith = Operator' "BeginsWith"

pattern Operator_Contains :: Operator
pattern Operator_Contains = Operator' "Contains"

pattern Operator_Equals :: Operator
pattern Operator_Equals = Operator' "Equals"

pattern Operator_GreaterThan :: Operator
pattern Operator_GreaterThan = Operator' "GreaterThan"

pattern Operator_GreaterThanOrEqual :: Operator
pattern Operator_GreaterThanOrEqual = Operator' "GreaterThanOrEqual"

pattern Operator_In :: Operator
pattern Operator_In = Operator' "In"

pattern Operator_LessThan :: Operator
pattern Operator_LessThan = Operator' "LessThan"

pattern Operator_LessThanOrEqual :: Operator
pattern Operator_LessThanOrEqual = Operator' "LessThanOrEqual"

pattern Operator_NotContains :: Operator
pattern Operator_NotContains = Operator' "NotContains"

pattern Operator_NotEquals :: Operator
pattern Operator_NotEquals = Operator' "NotEquals"

{-# COMPLETE
  Operator_BeginsWith,
  Operator_Contains,
  Operator_Equals,
  Operator_GreaterThan,
  Operator_GreaterThanOrEqual,
  Operator_In,
  Operator_LessThan,
  Operator_LessThanOrEqual,
  Operator_NotContains,
  Operator_NotEquals,
  Operator'
  #-}
