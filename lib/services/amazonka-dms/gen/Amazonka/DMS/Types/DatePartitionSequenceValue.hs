{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.DMS.Types.DatePartitionSequenceValue
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DMS.Types.DatePartitionSequenceValue
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype DatePartitionSequenceValue = DatePartitionSequenceValue'
  { fromDatePartitionSequenceValue ::
      Data.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
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
