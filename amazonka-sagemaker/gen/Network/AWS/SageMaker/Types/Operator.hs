{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.Operator
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.Operator
  ( Operator
      ( ..,
        Operator_Contains,
        Operator_Equals,
        Operator_Exists,
        Operator_GreaterThan,
        Operator_GreaterThanOrEqualTo,
        Operator_In,
        Operator_LessThan,
        Operator_LessThanOrEqualTo,
        Operator_NotEquals,
        Operator_NotExists
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype Operator = Operator'
  { fromOperator ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
    )

pattern Operator_Contains :: Operator
pattern Operator_Contains = Operator' "Contains"

pattern Operator_Equals :: Operator
pattern Operator_Equals = Operator' "Equals"

pattern Operator_Exists :: Operator
pattern Operator_Exists = Operator' "Exists"

pattern Operator_GreaterThan :: Operator
pattern Operator_GreaterThan = Operator' "GreaterThan"

pattern Operator_GreaterThanOrEqualTo :: Operator
pattern Operator_GreaterThanOrEqualTo = Operator' "GreaterThanOrEqualTo"

pattern Operator_In :: Operator
pattern Operator_In = Operator' "In"

pattern Operator_LessThan :: Operator
pattern Operator_LessThan = Operator' "LessThan"

pattern Operator_LessThanOrEqualTo :: Operator
pattern Operator_LessThanOrEqualTo = Operator' "LessThanOrEqualTo"

pattern Operator_NotEquals :: Operator
pattern Operator_NotEquals = Operator' "NotEquals"

pattern Operator_NotExists :: Operator
pattern Operator_NotExists = Operator' "NotExists"

{-# COMPLETE
  Operator_Contains,
  Operator_Equals,
  Operator_Exists,
  Operator_GreaterThan,
  Operator_GreaterThanOrEqualTo,
  Operator_In,
  Operator_LessThan,
  Operator_LessThanOrEqualTo,
  Operator_NotEquals,
  Operator_NotExists,
  Operator'
  #-}
