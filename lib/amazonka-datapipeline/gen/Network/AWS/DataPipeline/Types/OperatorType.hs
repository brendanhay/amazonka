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
        OperatorBetween,
        OperatorEQ,
        OperatorGE,
        OperatorLE,
        OperatorRefEQ
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype OperatorType = OperatorType' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern OperatorBetween :: OperatorType
pattern OperatorBetween = OperatorType' "BETWEEN"

pattern OperatorEQ :: OperatorType
pattern OperatorEQ = OperatorType' "EQ"

pattern OperatorGE :: OperatorType
pattern OperatorGE = OperatorType' "GE"

pattern OperatorLE :: OperatorType
pattern OperatorLE = OperatorType' "LE"

pattern OperatorRefEQ :: OperatorType
pattern OperatorRefEQ = OperatorType' "REF_EQ"

{-# COMPLETE
  OperatorBetween,
  OperatorEQ,
  OperatorGE,
  OperatorLE,
  OperatorRefEQ,
  OperatorType'
  #-}
