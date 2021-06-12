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
-- Module      : Network.AWS.EC2.Types.CancelBatchErrorCode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.CancelBatchErrorCode
  ( CancelBatchErrorCode
      ( ..,
        CancelBatchErrorCode_FleetRequestIdDoesNotExist,
        CancelBatchErrorCode_FleetRequestIdMalformed,
        CancelBatchErrorCode_FleetRequestNotInCancellableState,
        CancelBatchErrorCode_UnexpectedError
      ),
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal

newtype CancelBatchErrorCode = CancelBatchErrorCode'
  { fromCancelBatchErrorCode ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
    )

pattern CancelBatchErrorCode_FleetRequestIdDoesNotExist :: CancelBatchErrorCode
pattern CancelBatchErrorCode_FleetRequestIdDoesNotExist = CancelBatchErrorCode' "fleetRequestIdDoesNotExist"

pattern CancelBatchErrorCode_FleetRequestIdMalformed :: CancelBatchErrorCode
pattern CancelBatchErrorCode_FleetRequestIdMalformed = CancelBatchErrorCode' "fleetRequestIdMalformed"

pattern CancelBatchErrorCode_FleetRequestNotInCancellableState :: CancelBatchErrorCode
pattern CancelBatchErrorCode_FleetRequestNotInCancellableState = CancelBatchErrorCode' "fleetRequestNotInCancellableState"

pattern CancelBatchErrorCode_UnexpectedError :: CancelBatchErrorCode
pattern CancelBatchErrorCode_UnexpectedError = CancelBatchErrorCode' "unexpectedError"

{-# COMPLETE
  CancelBatchErrorCode_FleetRequestIdDoesNotExist,
  CancelBatchErrorCode_FleetRequestIdMalformed,
  CancelBatchErrorCode_FleetRequestNotInCancellableState,
  CancelBatchErrorCode_UnexpectedError,
  CancelBatchErrorCode'
  #-}
