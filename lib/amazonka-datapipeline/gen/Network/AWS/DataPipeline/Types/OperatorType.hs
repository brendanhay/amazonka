{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DataPipeline.Types.OperatorType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DataPipeline.Types.OperatorType
  ( OperatorType
      ( OperatorType',
        OperatorTypeOperatorEQ,
        OperatorTypeOperatorRefEq,
        OperatorTypeOperatorLE,
        OperatorTypeOperatorGE,
        OperatorTypeOperatorBetween,
        fromOperatorType
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype OperatorType = OperatorType' {fromOperatorType :: Core.Text}
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

pattern OperatorTypeOperatorEQ :: OperatorType
pattern OperatorTypeOperatorEQ = OperatorType' "EQ"

pattern OperatorTypeOperatorRefEq :: OperatorType
pattern OperatorTypeOperatorRefEq = OperatorType' "REF_EQ"

pattern OperatorTypeOperatorLE :: OperatorType
pattern OperatorTypeOperatorLE = OperatorType' "LE"

pattern OperatorTypeOperatorGE :: OperatorType
pattern OperatorTypeOperatorGE = OperatorType' "GE"

pattern OperatorTypeOperatorBetween :: OperatorType
pattern OperatorTypeOperatorBetween = OperatorType' "BETWEEN"

{-# COMPLETE
  OperatorTypeOperatorEQ,
  OperatorTypeOperatorRefEq,
  OperatorTypeOperatorLE,
  OperatorTypeOperatorGE,
  OperatorTypeOperatorBetween,
  OperatorType'
  #-}
