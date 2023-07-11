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
-- Module      : Amazonka.Redshift.Types.OperatorType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Redshift.Types.OperatorType
  ( OperatorType
      ( ..,
        OperatorType_Between,
        OperatorType_Eq,
        OperatorType_Ge,
        OperatorType_Gt,
        OperatorType_In,
        OperatorType_Le,
        OperatorType_Lt
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Internal

newtype OperatorType = OperatorType'
  { fromOperatorType ::
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

pattern OperatorType_Between :: OperatorType
pattern OperatorType_Between = OperatorType' "between"

pattern OperatorType_Eq :: OperatorType
pattern OperatorType_Eq = OperatorType' "eq"

pattern OperatorType_Ge :: OperatorType
pattern OperatorType_Ge = OperatorType' "ge"

pattern OperatorType_Gt :: OperatorType
pattern OperatorType_Gt = OperatorType' "gt"

pattern OperatorType_In :: OperatorType
pattern OperatorType_In = OperatorType' "in"

pattern OperatorType_Le :: OperatorType
pattern OperatorType_Le = OperatorType' "le"

pattern OperatorType_Lt :: OperatorType
pattern OperatorType_Lt = OperatorType' "lt"

{-# COMPLETE
  OperatorType_Between,
  OperatorType_Eq,
  OperatorType_Ge,
  OperatorType_Gt,
  OperatorType_In,
  OperatorType_Le,
  OperatorType_Lt,
  OperatorType'
  #-}
