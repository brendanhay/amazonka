{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
        RuleTypeBinaryLength,
        RuleTypeNumberComparison,
        RuleTypeStringFromSet,
        RuleTypeStringLength,
        fromRuleType
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype RuleType = RuleType' {fromRuleType :: Core.Text}
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

pattern RuleTypeBinaryLength :: RuleType
pattern RuleTypeBinaryLength = RuleType' "BINARY_LENGTH"

pattern RuleTypeNumberComparison :: RuleType
pattern RuleTypeNumberComparison = RuleType' "NUMBER_COMPARISON"

pattern RuleTypeStringFromSet :: RuleType
pattern RuleTypeStringFromSet = RuleType' "STRING_FROM_SET"

pattern RuleTypeStringLength :: RuleType
pattern RuleTypeStringLength = RuleType' "STRING_LENGTH"

{-# COMPLETE
  RuleTypeBinaryLength,
  RuleTypeNumberComparison,
  RuleTypeStringFromSet,
  RuleTypeStringLength,
  RuleType'
  #-}
