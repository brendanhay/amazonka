{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.OpsFilterOperatorType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.OpsFilterOperatorType
  ( OpsFilterOperatorType
      ( OpsFilterOperatorType',
        OpsFilterOperatorTypeEqual,
        OpsFilterOperatorTypeNotEqual,
        OpsFilterOperatorTypeBeginWith,
        OpsFilterOperatorTypeLessThan,
        OpsFilterOperatorTypeGreaterThan,
        OpsFilterOperatorTypeExists,
        fromOpsFilterOperatorType
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype OpsFilterOperatorType = OpsFilterOperatorType'
  { fromOpsFilterOperatorType ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern OpsFilterOperatorTypeEqual :: OpsFilterOperatorType
pattern OpsFilterOperatorTypeEqual = OpsFilterOperatorType' "Equal"

pattern OpsFilterOperatorTypeNotEqual :: OpsFilterOperatorType
pattern OpsFilterOperatorTypeNotEqual = OpsFilterOperatorType' "NotEqual"

pattern OpsFilterOperatorTypeBeginWith :: OpsFilterOperatorType
pattern OpsFilterOperatorTypeBeginWith = OpsFilterOperatorType' "BeginWith"

pattern OpsFilterOperatorTypeLessThan :: OpsFilterOperatorType
pattern OpsFilterOperatorTypeLessThan = OpsFilterOperatorType' "LessThan"

pattern OpsFilterOperatorTypeGreaterThan :: OpsFilterOperatorType
pattern OpsFilterOperatorTypeGreaterThan = OpsFilterOperatorType' "GreaterThan"

pattern OpsFilterOperatorTypeExists :: OpsFilterOperatorType
pattern OpsFilterOperatorTypeExists = OpsFilterOperatorType' "Exists"

{-# COMPLETE
  OpsFilterOperatorTypeEqual,
  OpsFilterOperatorTypeNotEqual,
  OpsFilterOperatorTypeBeginWith,
  OpsFilterOperatorTypeLessThan,
  OpsFilterOperatorTypeGreaterThan,
  OpsFilterOperatorTypeExists,
  OpsFilterOperatorType'
  #-}
