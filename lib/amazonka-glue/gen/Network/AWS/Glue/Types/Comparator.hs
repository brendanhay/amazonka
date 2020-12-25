{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.Comparator
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.Comparator
  ( Comparator
      ( Comparator',
        ComparatorEquals,
        ComparatorGreaterThan,
        ComparatorLessThan,
        ComparatorGreaterThanEquals,
        ComparatorLessThanEquals,
        fromComparator
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype Comparator = Comparator' {fromComparator :: Core.Text}
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

pattern ComparatorEquals :: Comparator
pattern ComparatorEquals = Comparator' "EQUALS"

pattern ComparatorGreaterThan :: Comparator
pattern ComparatorGreaterThan = Comparator' "GREATER_THAN"

pattern ComparatorLessThan :: Comparator
pattern ComparatorLessThan = Comparator' "LESS_THAN"

pattern ComparatorGreaterThanEquals :: Comparator
pattern ComparatorGreaterThanEquals = Comparator' "GREATER_THAN_EQUALS"

pattern ComparatorLessThanEquals :: Comparator
pattern ComparatorLessThanEquals = Comparator' "LESS_THAN_EQUALS"

{-# COMPLETE
  ComparatorEquals,
  ComparatorGreaterThan,
  ComparatorLessThan,
  ComparatorGreaterThanEquals,
  ComparatorLessThanEquals,
  Comparator'
  #-}
