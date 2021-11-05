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
-- Module      : Network.AWS.EMRContainers.Types.FailureReason
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMRContainers.Types.FailureReason
  ( FailureReason
      ( ..,
        FailureReason_CLUSTER_UNAVAILABLE,
        FailureReason_INTERNAL_ERROR,
        FailureReason_USER_ERROR,
        FailureReason_VALIDATION_ERROR
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype FailureReason = FailureReason'
  { fromFailureReason ::
      Core.Text
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
