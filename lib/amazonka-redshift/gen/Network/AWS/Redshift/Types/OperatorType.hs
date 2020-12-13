{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.OperatorType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.OperatorType
  ( OperatorType
      ( OperatorType',
        EQ,
        LT,
        GT,
        LE,
        GE,
        IN,
        Between
      ),
  )
where

import qualified Network.AWS.Prelude as Lude
import Network.AWS.Redshift.Internal

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

pattern EQ :: OperatorType
pattern EQ = OperatorType' "eq"

pattern LT :: OperatorType
pattern LT = OperatorType' "lt"

pattern GT :: OperatorType
pattern GT = OperatorType' "gt"

pattern LE :: OperatorType
pattern LE = OperatorType' "le"

pattern GE :: OperatorType
pattern GE = OperatorType' "ge"

pattern IN :: OperatorType
pattern IN = OperatorType' "in"

pattern Between :: OperatorType
pattern Between = OperatorType' "between"

{-# COMPLETE
  EQ,
  LT,
  GT,
  LE,
  GE,
  IN,
  Between,
  OperatorType'
  #-}
