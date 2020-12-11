-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.DatePartitionSequenceValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.DatePartitionSequenceValue
  ( DatePartitionSequenceValue
      ( DatePartitionSequenceValue',
        Ddmmyyyy,
        Mmyyyydd,
        Yyyymm,
        Yyyymmdd,
        Yyyymmddhh
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype DatePartitionSequenceValue = DatePartitionSequenceValue' Lude.Text
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

pattern Ddmmyyyy :: DatePartitionSequenceValue
pattern Ddmmyyyy = DatePartitionSequenceValue' "DDMMYYYY"

pattern Mmyyyydd :: DatePartitionSequenceValue
pattern Mmyyyydd = DatePartitionSequenceValue' "MMYYYYDD"

pattern Yyyymm :: DatePartitionSequenceValue
pattern Yyyymm = DatePartitionSequenceValue' "YYYYMM"

pattern Yyyymmdd :: DatePartitionSequenceValue
pattern Yyyymmdd = DatePartitionSequenceValue' "YYYYMMDD"

pattern Yyyymmddhh :: DatePartitionSequenceValue
pattern Yyyymmddhh = DatePartitionSequenceValue' "YYYYMMDDHH"

{-# COMPLETE
  Ddmmyyyy,
  Mmyyyydd,
  Yyyymm,
  Yyyymmdd,
  Yyyymmddhh,
  DatePartitionSequenceValue'
  #-}
