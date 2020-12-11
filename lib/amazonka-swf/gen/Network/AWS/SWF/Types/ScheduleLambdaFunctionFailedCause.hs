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
        IdAlreadyInUse,
        LambdaFunctionCreationRateExceeded,
        LambdaServiceNotAvailableInRegion,
        OpenLambdaFunctionsLimitExceeded
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ScheduleLambdaFunctionFailedCause = ScheduleLambdaFunctionFailedCause' Lude.Text
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

pattern IdAlreadyInUse :: ScheduleLambdaFunctionFailedCause
pattern IdAlreadyInUse = ScheduleLambdaFunctionFailedCause' "ID_ALREADY_IN_USE"

pattern LambdaFunctionCreationRateExceeded :: ScheduleLambdaFunctionFailedCause
pattern LambdaFunctionCreationRateExceeded = ScheduleLambdaFunctionFailedCause' "LAMBDA_FUNCTION_CREATION_RATE_EXCEEDED"

pattern LambdaServiceNotAvailableInRegion :: ScheduleLambdaFunctionFailedCause
pattern LambdaServiceNotAvailableInRegion = ScheduleLambdaFunctionFailedCause' "LAMBDA_SERVICE_NOT_AVAILABLE_IN_REGION"

pattern OpenLambdaFunctionsLimitExceeded :: ScheduleLambdaFunctionFailedCause
pattern OpenLambdaFunctionsLimitExceeded = ScheduleLambdaFunctionFailedCause' "OPEN_LAMBDA_FUNCTIONS_LIMIT_EXCEEDED"

{-# COMPLETE
  IdAlreadyInUse,
  LambdaFunctionCreationRateExceeded,
  LambdaServiceNotAvailableInRegion,
  OpenLambdaFunctionsLimitExceeded,
  ScheduleLambdaFunctionFailedCause'
  #-}
