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
-- Module      : Amazonka.IoTEventsData.Types.ErrorCode
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTEventsData.Types.ErrorCode
  ( ErrorCode
      ( ..,
        ErrorCode_InternalFailureException,
        ErrorCode_InvalidRequestException,
        ErrorCode_ResourceNotFoundException,
        ErrorCode_ServiceUnavailableException,
        ErrorCode_ThrottlingException
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ErrorCode = ErrorCode'
  { fromErrorCode ::
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

pattern ErrorCode_InternalFailureException :: ErrorCode
pattern ErrorCode_InternalFailureException = ErrorCode' "InternalFailureException"

pattern ErrorCode_InvalidRequestException :: ErrorCode
pattern ErrorCode_InvalidRequestException = ErrorCode' "InvalidRequestException"

pattern ErrorCode_ResourceNotFoundException :: ErrorCode
pattern ErrorCode_ResourceNotFoundException = ErrorCode' "ResourceNotFoundException"

pattern ErrorCode_ServiceUnavailableException :: ErrorCode
pattern ErrorCode_ServiceUnavailableException = ErrorCode' "ServiceUnavailableException"

pattern ErrorCode_ThrottlingException :: ErrorCode
pattern ErrorCode_ThrottlingException = ErrorCode' "ThrottlingException"

{-# COMPLETE
  ErrorCode_InternalFailureException,
  ErrorCode_InvalidRequestException,
  ErrorCode_ResourceNotFoundException,
  ErrorCode_ServiceUnavailableException,
  ErrorCode_ThrottlingException,
  ErrorCode'
  #-}
