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
-- Module      : Amazonka.ConnectCampaigns.Types.FailureCode
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ConnectCampaigns.Types.FailureCode
  ( FailureCode
      ( ..,
        FailureCode_InvalidInput,
        FailureCode_RequestThrottled,
        FailureCode_UnknownError
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A predefined code indicating the error that caused the failure.
newtype FailureCode = FailureCode'
  { fromFailureCode ::
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

pattern FailureCode_InvalidInput :: FailureCode
pattern FailureCode_InvalidInput = FailureCode' "InvalidInput"

pattern FailureCode_RequestThrottled :: FailureCode
pattern FailureCode_RequestThrottled = FailureCode' "RequestThrottled"

pattern FailureCode_UnknownError :: FailureCode
pattern FailureCode_UnknownError = FailureCode' "UnknownError"

{-# COMPLETE
  FailureCode_InvalidInput,
  FailureCode_RequestThrottled,
  FailureCode_UnknownError,
  FailureCode'
  #-}
