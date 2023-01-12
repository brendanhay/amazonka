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
-- Module      : Amazonka.SageMakerMetrics.Types.PutMetricsErrorCode
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMakerMetrics.Types.PutMetricsErrorCode
  ( PutMetricsErrorCode
      ( ..,
        PutMetricsErrorCode_CONFLICT_ERROR,
        PutMetricsErrorCode_INTERNAL_ERROR,
        PutMetricsErrorCode_METRIC_LIMIT_EXCEEDED,
        PutMetricsErrorCode_VALIDATION_ERROR
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype PutMetricsErrorCode = PutMetricsErrorCode'
  { fromPutMetricsErrorCode ::
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

pattern PutMetricsErrorCode_CONFLICT_ERROR :: PutMetricsErrorCode
pattern PutMetricsErrorCode_CONFLICT_ERROR = PutMetricsErrorCode' "CONFLICT_ERROR"

pattern PutMetricsErrorCode_INTERNAL_ERROR :: PutMetricsErrorCode
pattern PutMetricsErrorCode_INTERNAL_ERROR = PutMetricsErrorCode' "INTERNAL_ERROR"

pattern PutMetricsErrorCode_METRIC_LIMIT_EXCEEDED :: PutMetricsErrorCode
pattern PutMetricsErrorCode_METRIC_LIMIT_EXCEEDED = PutMetricsErrorCode' "METRIC_LIMIT_EXCEEDED"

pattern PutMetricsErrorCode_VALIDATION_ERROR :: PutMetricsErrorCode
pattern PutMetricsErrorCode_VALIDATION_ERROR = PutMetricsErrorCode' "VALIDATION_ERROR"

{-# COMPLETE
  PutMetricsErrorCode_CONFLICT_ERROR,
  PutMetricsErrorCode_INTERNAL_ERROR,
  PutMetricsErrorCode_METRIC_LIMIT_EXCEEDED,
  PutMetricsErrorCode_VALIDATION_ERROR,
  PutMetricsErrorCode'
  #-}
