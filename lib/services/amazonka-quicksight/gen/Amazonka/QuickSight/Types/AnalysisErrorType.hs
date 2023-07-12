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
-- Module      : Amazonka.QuickSight.Types.AnalysisErrorType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.AnalysisErrorType
  ( AnalysisErrorType
      ( ..,
        AnalysisErrorType_ACCESS_DENIED,
        AnalysisErrorType_COLUMN_GEOGRAPHIC_ROLE_MISMATCH,
        AnalysisErrorType_COLUMN_REPLACEMENT_MISSING,
        AnalysisErrorType_COLUMN_TYPE_MISMATCH,
        AnalysisErrorType_DATA_SET_NOT_FOUND,
        AnalysisErrorType_INTERNAL_FAILURE,
        AnalysisErrorType_PARAMETER_NOT_FOUND,
        AnalysisErrorType_PARAMETER_TYPE_INVALID,
        AnalysisErrorType_PARAMETER_VALUE_INCOMPATIBLE,
        AnalysisErrorType_SOURCE_NOT_FOUND
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype AnalysisErrorType = AnalysisErrorType'
  { fromAnalysisErrorType ::
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

pattern AnalysisErrorType_ACCESS_DENIED :: AnalysisErrorType
pattern AnalysisErrorType_ACCESS_DENIED = AnalysisErrorType' "ACCESS_DENIED"

pattern AnalysisErrorType_COLUMN_GEOGRAPHIC_ROLE_MISMATCH :: AnalysisErrorType
pattern AnalysisErrorType_COLUMN_GEOGRAPHIC_ROLE_MISMATCH = AnalysisErrorType' "COLUMN_GEOGRAPHIC_ROLE_MISMATCH"

pattern AnalysisErrorType_COLUMN_REPLACEMENT_MISSING :: AnalysisErrorType
pattern AnalysisErrorType_COLUMN_REPLACEMENT_MISSING = AnalysisErrorType' "COLUMN_REPLACEMENT_MISSING"

pattern AnalysisErrorType_COLUMN_TYPE_MISMATCH :: AnalysisErrorType
pattern AnalysisErrorType_COLUMN_TYPE_MISMATCH = AnalysisErrorType' "COLUMN_TYPE_MISMATCH"

pattern AnalysisErrorType_DATA_SET_NOT_FOUND :: AnalysisErrorType
pattern AnalysisErrorType_DATA_SET_NOT_FOUND = AnalysisErrorType' "DATA_SET_NOT_FOUND"

pattern AnalysisErrorType_INTERNAL_FAILURE :: AnalysisErrorType
pattern AnalysisErrorType_INTERNAL_FAILURE = AnalysisErrorType' "INTERNAL_FAILURE"

pattern AnalysisErrorType_PARAMETER_NOT_FOUND :: AnalysisErrorType
pattern AnalysisErrorType_PARAMETER_NOT_FOUND = AnalysisErrorType' "PARAMETER_NOT_FOUND"

pattern AnalysisErrorType_PARAMETER_TYPE_INVALID :: AnalysisErrorType
pattern AnalysisErrorType_PARAMETER_TYPE_INVALID = AnalysisErrorType' "PARAMETER_TYPE_INVALID"

pattern AnalysisErrorType_PARAMETER_VALUE_INCOMPATIBLE :: AnalysisErrorType
pattern AnalysisErrorType_PARAMETER_VALUE_INCOMPATIBLE = AnalysisErrorType' "PARAMETER_VALUE_INCOMPATIBLE"

pattern AnalysisErrorType_SOURCE_NOT_FOUND :: AnalysisErrorType
pattern AnalysisErrorType_SOURCE_NOT_FOUND = AnalysisErrorType' "SOURCE_NOT_FOUND"

{-# COMPLETE
  AnalysisErrorType_ACCESS_DENIED,
  AnalysisErrorType_COLUMN_GEOGRAPHIC_ROLE_MISMATCH,
  AnalysisErrorType_COLUMN_REPLACEMENT_MISSING,
  AnalysisErrorType_COLUMN_TYPE_MISMATCH,
  AnalysisErrorType_DATA_SET_NOT_FOUND,
  AnalysisErrorType_INTERNAL_FAILURE,
  AnalysisErrorType_PARAMETER_NOT_FOUND,
  AnalysisErrorType_PARAMETER_TYPE_INVALID,
  AnalysisErrorType_PARAMETER_VALUE_INCOMPATIBLE,
  AnalysisErrorType_SOURCE_NOT_FOUND,
  AnalysisErrorType'
  #-}
