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
-- Module      : Amazonka.SWF.Types.ScheduleLambdaFunctionFailedCause
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SWF.Types.ScheduleLambdaFunctionFailedCause
  ( ScheduleLambdaFunctionFailedCause
      ( ..,
        ScheduleLambdaFunctionFailedCause_ID_ALREADY_IN_USE,
        ScheduleLambdaFunctionFailedCause_LAMBDA_FUNCTION_CREATION_RATE_EXCEEDED,
        ScheduleLambdaFunctionFailedCause_LAMBDA_SERVICE_NOT_AVAILABLE_IN_REGION,
        ScheduleLambdaFunctionFailedCause_OPEN_LAMBDA_FUNCTIONS_LIMIT_EXCEEDED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ScheduleLambdaFunctionFailedCause = ScheduleLambdaFunctionFailedCause'
  { fromScheduleLambdaFunctionFailedCause ::
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

pattern ScheduleLambdaFunctionFailedCause_ID_ALREADY_IN_USE :: ScheduleLambdaFunctionFailedCause
pattern ScheduleLambdaFunctionFailedCause_ID_ALREADY_IN_USE = ScheduleLambdaFunctionFailedCause' "ID_ALREADY_IN_USE"

pattern ScheduleLambdaFunctionFailedCause_LAMBDA_FUNCTION_CREATION_RATE_EXCEEDED :: ScheduleLambdaFunctionFailedCause
pattern ScheduleLambdaFunctionFailedCause_LAMBDA_FUNCTION_CREATION_RATE_EXCEEDED = ScheduleLambdaFunctionFailedCause' "LAMBDA_FUNCTION_CREATION_RATE_EXCEEDED"

pattern ScheduleLambdaFunctionFailedCause_LAMBDA_SERVICE_NOT_AVAILABLE_IN_REGION :: ScheduleLambdaFunctionFailedCause
pattern ScheduleLambdaFunctionFailedCause_LAMBDA_SERVICE_NOT_AVAILABLE_IN_REGION = ScheduleLambdaFunctionFailedCause' "LAMBDA_SERVICE_NOT_AVAILABLE_IN_REGION"

pattern ScheduleLambdaFunctionFailedCause_OPEN_LAMBDA_FUNCTIONS_LIMIT_EXCEEDED :: ScheduleLambdaFunctionFailedCause
pattern ScheduleLambdaFunctionFailedCause_OPEN_LAMBDA_FUNCTIONS_LIMIT_EXCEEDED = ScheduleLambdaFunctionFailedCause' "OPEN_LAMBDA_FUNCTIONS_LIMIT_EXCEEDED"

{-# COMPLETE
  ScheduleLambdaFunctionFailedCause_ID_ALREADY_IN_USE,
  ScheduleLambdaFunctionFailedCause_LAMBDA_FUNCTION_CREATION_RATE_EXCEEDED,
  ScheduleLambdaFunctionFailedCause_LAMBDA_SERVICE_NOT_AVAILABLE_IN_REGION,
  ScheduleLambdaFunctionFailedCause_OPEN_LAMBDA_FUNCTIONS_LIMIT_EXCEEDED,
  ScheduleLambdaFunctionFailedCause'
  #-}
