{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.ScheduleLambdaFunctionFailedCause
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.ScheduleLambdaFunctionFailedCause
  ( ScheduleLambdaFunctionFailedCause
      ( ScheduleLambdaFunctionFailedCause',
        ScheduleLambdaFunctionFailedCauseIdAlreadyInUse,
        ScheduleLambdaFunctionFailedCauseOpenLambdaFunctionsLimitExceeded,
        ScheduleLambdaFunctionFailedCauseLambdaFunctionCreationRateExceeded,
        ScheduleLambdaFunctionFailedCauseLambdaServiceNotAvailableInRegion,
        fromScheduleLambdaFunctionFailedCause
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype ScheduleLambdaFunctionFailedCause = ScheduleLambdaFunctionFailedCause'
  { fromScheduleLambdaFunctionFailedCause ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern ScheduleLambdaFunctionFailedCauseIdAlreadyInUse :: ScheduleLambdaFunctionFailedCause
pattern ScheduleLambdaFunctionFailedCauseIdAlreadyInUse = ScheduleLambdaFunctionFailedCause' "ID_ALREADY_IN_USE"

pattern ScheduleLambdaFunctionFailedCauseOpenLambdaFunctionsLimitExceeded :: ScheduleLambdaFunctionFailedCause
pattern ScheduleLambdaFunctionFailedCauseOpenLambdaFunctionsLimitExceeded = ScheduleLambdaFunctionFailedCause' "OPEN_LAMBDA_FUNCTIONS_LIMIT_EXCEEDED"

pattern ScheduleLambdaFunctionFailedCauseLambdaFunctionCreationRateExceeded :: ScheduleLambdaFunctionFailedCause
pattern ScheduleLambdaFunctionFailedCauseLambdaFunctionCreationRateExceeded = ScheduleLambdaFunctionFailedCause' "LAMBDA_FUNCTION_CREATION_RATE_EXCEEDED"

pattern ScheduleLambdaFunctionFailedCauseLambdaServiceNotAvailableInRegion :: ScheduleLambdaFunctionFailedCause
pattern ScheduleLambdaFunctionFailedCauseLambdaServiceNotAvailableInRegion = ScheduleLambdaFunctionFailedCause' "LAMBDA_SERVICE_NOT_AVAILABLE_IN_REGION"

{-# COMPLETE
  ScheduleLambdaFunctionFailedCauseIdAlreadyInUse,
  ScheduleLambdaFunctionFailedCauseOpenLambdaFunctionsLimitExceeded,
  ScheduleLambdaFunctionFailedCauseLambdaFunctionCreationRateExceeded,
  ScheduleLambdaFunctionFailedCauseLambdaServiceNotAvailableInRegion,
  ScheduleLambdaFunctionFailedCause'
  #-}
