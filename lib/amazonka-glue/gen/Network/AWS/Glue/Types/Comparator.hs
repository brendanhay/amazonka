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
        CEquals,
        CGreaterThan,
        CGreaterThanEquals,
        CLessThan,
        CLessThanEquals
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

pattern CEquals :: Comparator
pattern CEquals = Comparator' "EQUALS"

pattern CGreaterThan :: Comparator
pattern CGreaterThan = Comparator' "GREATER_THAN"

pattern CGreaterThanEquals :: Comparator
pattern CGreaterThanEquals = Comparator' "GREATER_THAN_EQUALS"

pattern CLessThan :: Comparator
pattern CLessThan = Comparator' "LESS_THAN"

pattern CLessThanEquals :: Comparator
pattern CLessThanEquals = Comparator' "LESS_THAN_EQUALS"

{-# COMPLETE
  CEquals,
  CGreaterThan,
  CGreaterThanEquals,
  CLessThan,
  CLessThanEquals,
  Comparator'
  #-}
