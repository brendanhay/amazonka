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
-- Module      : Amazonka.EMRContainers.Types.FailureReason
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMRContainers.Types.FailureReason
  ( FailureReason
      ( ..,
        FailureReason_CLUSTER_UNAVAILABLE,
        FailureReason_INTERNAL_ERROR,
        FailureReason_USER_ERROR,
        FailureReason_VALIDATION_ERROR
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype FailureReason = FailureReason'
  { fromFailureReason ::
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

pattern FailureReason_CLUSTER_UNAVAILABLE :: FailureReason
pattern FailureReason_CLUSTER_UNAVAILABLE = FailureReason' "CLUSTER_UNAVAILABLE"

pattern FailureReason_INTERNAL_ERROR :: FailureReason
pattern FailureReason_INTERNAL_ERROR = FailureReason' "INTERNAL_ERROR"

pattern FailureReason_USER_ERROR :: FailureReason
pattern FailureReason_USER_ERROR = FailureReason' "USER_ERROR"

pattern FailureReason_VALIDATION_ERROR :: FailureReason
pattern FailureReason_VALIDATION_ERROR = FailureReason' "VALIDATION_ERROR"

{-# COMPLETE
  FailureReason_CLUSTER_UNAVAILABLE,
  FailureReason_INTERNAL_ERROR,
  FailureReason_USER_ERROR,
  FailureReason_VALIDATION_ERROR,
  FailureReason'
  #-}
