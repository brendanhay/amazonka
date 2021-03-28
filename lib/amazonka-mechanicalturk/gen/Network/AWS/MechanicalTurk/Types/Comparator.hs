{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.Types.Comparator
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MechanicalTurk.Types.Comparator
  ( Comparator
    ( Comparator'
    , ComparatorLessThan
    , ComparatorLessThanOrEqualTo
    , ComparatorGreaterThan
    , ComparatorGreaterThanOrEqualTo
    , ComparatorEqualTo
    , ComparatorNotEqualTo
    , ComparatorExists
    , ComparatorDoesNotExist
    , ComparatorIN
    , ComparatorNotIn
    , fromComparator
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype Comparator = Comparator'{fromComparator :: Core.Text}
                       deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                       Core.Generic)
                       deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                         Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                         Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                         Core.FromText, Core.ToByteString, Core.ToQuery,
                                         Core.ToHeader)

pattern ComparatorLessThan :: Comparator
pattern ComparatorLessThan = Comparator' "LessThan"

pattern ComparatorLessThanOrEqualTo :: Comparator
pattern ComparatorLessThanOrEqualTo = Comparator' "LessThanOrEqualTo"

pattern ComparatorGreaterThan :: Comparator
pattern ComparatorGreaterThan = Comparator' "GreaterThan"

pattern ComparatorGreaterThanOrEqualTo :: Comparator
pattern ComparatorGreaterThanOrEqualTo = Comparator' "GreaterThanOrEqualTo"

pattern ComparatorEqualTo :: Comparator
pattern ComparatorEqualTo = Comparator' "EqualTo"

pattern ComparatorNotEqualTo :: Comparator
pattern ComparatorNotEqualTo = Comparator' "NotEqualTo"

pattern ComparatorExists :: Comparator
pattern ComparatorExists = Comparator' "Exists"

pattern ComparatorDoesNotExist :: Comparator
pattern ComparatorDoesNotExist = Comparator' "DoesNotExist"

pattern ComparatorIN :: Comparator
pattern ComparatorIN = Comparator' "In"

pattern ComparatorNotIn :: Comparator
pattern ComparatorNotIn = Comparator' "NotIn"

{-# COMPLETE 
  ComparatorLessThan,

  ComparatorLessThanOrEqualTo,

  ComparatorGreaterThan,

  ComparatorGreaterThanOrEqualTo,

  ComparatorEqualTo,

  ComparatorNotEqualTo,

  ComparatorExists,

  ComparatorDoesNotExist,

  ComparatorIN,

  ComparatorNotIn,
  Comparator'
  #-}
