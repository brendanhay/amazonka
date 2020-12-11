-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.RuleType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.RuleType
  ( RuleType
      ( RuleType',
        BinaryLength,
        NumberComparison,
        StringFromSet,
        StringLength
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype RuleType = RuleType' Lude.Text
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

pattern BinaryLength :: RuleType
pattern BinaryLength = RuleType' "BINARY_LENGTH"

pattern NumberComparison :: RuleType
pattern NumberComparison = RuleType' "NUMBER_COMPARISON"

pattern StringFromSet :: RuleType
pattern StringFromSet = RuleType' "STRING_FROM_SET"

pattern StringLength :: RuleType
pattern StringLength = RuleType' "STRING_LENGTH"

{-# COMPLETE
  BinaryLength,
  NumberComparison,
  StringFromSet,
  StringLength,
  RuleType'
  #-}
