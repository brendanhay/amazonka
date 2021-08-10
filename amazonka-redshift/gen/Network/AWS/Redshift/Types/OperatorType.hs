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
-- Module      : Network.AWS.Redshift.Types.OperatorType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.OperatorType
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Redshift.Internal

newtype OperatorType = OperatorType'
  { fromOperatorType ::
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
