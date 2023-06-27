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
-- Module      : Amazonka.CustomerProfiles.Types.Operator
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CustomerProfiles.Types.Operator
  ( Operator
      ( ..,
        Operator_EQUAL_TO,
        Operator_GREATER_THAN,
        Operator_LESS_THAN,
        Operator_NOT_EQUAL_TO
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

pattern Operator_EQUAL_TO :: Operator
pattern Operator_EQUAL_TO = Operator' "EQUAL_TO"

pattern Operator_GREATER_THAN :: Operator
pattern Operator_GREATER_THAN = Operator' "GREATER_THAN"

pattern Operator_LESS_THAN :: Operator
pattern Operator_LESS_THAN = Operator' "LESS_THAN"

pattern Operator_NOT_EQUAL_TO :: Operator
pattern Operator_NOT_EQUAL_TO = Operator' "NOT_EQUAL_TO"

{-# COMPLETE
  Operator_EQUAL_TO,
  Operator_GREATER_THAN,
  Operator_LESS_THAN,
  Operator_NOT_EQUAL_TO,
  Operator'
  #-}
