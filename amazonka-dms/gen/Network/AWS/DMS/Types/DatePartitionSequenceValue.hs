{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.DatePartitionSequenceValue
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.DatePartitionSequenceValue
  ( DatePartitionSequenceValue
      ( ..,
        DatePartitionSequenceValue_DDMMYYYY,
        DatePartitionSequenceValue_MMYYYYDD,
        DatePartitionSequenceValue_YYYYMM,
        DatePartitionSequenceValue_YYYYMMDD,
        DatePartitionSequenceValue_YYYYMMDDHH
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype DatePartitionSequenceValue = DatePartitionSequenceValue'
  { fromDatePartitionSequenceValue ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
    )

pattern DatePartitionSequenceValue_DDMMYYYY :: DatePartitionSequenceValue
pattern DatePartitionSequenceValue_DDMMYYYY = DatePartitionSequenceValue' "DDMMYYYY"

pattern DatePartitionSequenceValue_MMYYYYDD :: DatePartitionSequenceValue
pattern DatePartitionSequenceValue_MMYYYYDD = DatePartitionSequenceValue' "MMYYYYDD"

pattern DatePartitionSequenceValue_YYYYMM :: DatePartitionSequenceValue
pattern DatePartitionSequenceValue_YYYYMM = DatePartitionSequenceValue' "YYYYMM"

pattern DatePartitionSequenceValue_YYYYMMDD :: DatePartitionSequenceValue
pattern DatePartitionSequenceValue_YYYYMMDD = DatePartitionSequenceValue' "YYYYMMDD"

pattern DatePartitionSequenceValue_YYYYMMDDHH :: DatePartitionSequenceValue
pattern DatePartitionSequenceValue_YYYYMMDDHH = DatePartitionSequenceValue' "YYYYMMDDHH"

{-# COMPLETE
  DatePartitionSequenceValue_DDMMYYYY,
  DatePartitionSequenceValue_MMYYYYDD,
  DatePartitionSequenceValue_YYYYMM,
  DatePartitionSequenceValue_YYYYMMDD,
  DatePartitionSequenceValue_YYYYMMDDHH,
  DatePartitionSequenceValue'
  #-}
