{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.Types.Comparator
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MechanicalTurk.Types.Comparator
  ( Comparator
      ( Comparator',
        LessThan,
        LessThanOrEqualTo,
        GreaterThan,
        GreaterThanOrEqualTo,
        EqualTo,
        NotEqualTo,
        Exists,
        DoesNotExist,
        IN,
        NotIn
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype Comparator = Comparator' Lude.Text
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

pattern LessThan :: Comparator
pattern LessThan = Comparator' "LessThan"

pattern LessThanOrEqualTo :: Comparator
pattern LessThanOrEqualTo = Comparator' "LessThanOrEqualTo"

pattern GreaterThan :: Comparator
pattern GreaterThan = Comparator' "GreaterThan"

pattern GreaterThanOrEqualTo :: Comparator
pattern GreaterThanOrEqualTo = Comparator' "GreaterThanOrEqualTo"

pattern EqualTo :: Comparator
pattern EqualTo = Comparator' "EqualTo"

pattern NotEqualTo :: Comparator
pattern NotEqualTo = Comparator' "NotEqualTo"

pattern Exists :: Comparator
pattern Exists = Comparator' "Exists"

pattern DoesNotExist :: Comparator
pattern DoesNotExist = Comparator' "DoesNotExist"

pattern IN :: Comparator
pattern IN = Comparator' "In"

pattern NotIn :: Comparator
pattern NotIn = Comparator' "NotIn"

{-# COMPLETE
  LessThan,
  LessThanOrEqualTo,
  GreaterThan,
  GreaterThanOrEqualTo,
  EqualTo,
  NotEqualTo,
  Exists,
  DoesNotExist,
  IN,
  NotIn,
  Comparator'
  #-}
