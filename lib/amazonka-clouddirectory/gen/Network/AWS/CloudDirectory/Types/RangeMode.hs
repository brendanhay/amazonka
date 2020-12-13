{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.RangeMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.RangeMode
  ( RangeMode
      ( RangeMode',
        First,
        Last,
        LastBeforeMissingValues,
        Inclusive,
        Exclusive
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype RangeMode = RangeMode' Lude.Text
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

pattern First :: RangeMode
pattern First = RangeMode' "FIRST"

pattern Last :: RangeMode
pattern Last = RangeMode' "LAST"

pattern LastBeforeMissingValues :: RangeMode
pattern LastBeforeMissingValues = RangeMode' "LAST_BEFORE_MISSING_VALUES"

pattern Inclusive :: RangeMode
pattern Inclusive = RangeMode' "INCLUSIVE"

pattern Exclusive :: RangeMode
pattern Exclusive = RangeMode' "EXCLUSIVE"

{-# COMPLETE
  First,
  Last,
  LastBeforeMissingValues,
  Inclusive,
  Exclusive,
  RangeMode'
  #-}
