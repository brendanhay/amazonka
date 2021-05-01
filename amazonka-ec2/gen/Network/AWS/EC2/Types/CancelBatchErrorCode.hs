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

import Network.AWS.EC2.Internal
import qualified Network.AWS.Prelude as Prelude

newtype CancelBatchErrorCode = CancelBatchErrorCode'
  { fromCancelBatchErrorCode ::
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
