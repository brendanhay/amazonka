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
-- Module      : Amazonka.WellArchitected.Types.CheckFailureReason
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WellArchitected.Types.CheckFailureReason
  ( CheckFailureReason
      ( ..,
        CheckFailureReason_ACCESS_DENIED,
        CheckFailureReason_ASSUME_ROLE_ERROR,
        CheckFailureReason_PREMIUM_SUPPORT_REQUIRED,
        CheckFailureReason_UNKNOWN_ERROR
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype CheckFailureReason = CheckFailureReason'
  { fromCheckFailureReason ::
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

pattern CheckFailureReason_ACCESS_DENIED :: CheckFailureReason
pattern CheckFailureReason_ACCESS_DENIED = CheckFailureReason' "ACCESS_DENIED"

pattern CheckFailureReason_ASSUME_ROLE_ERROR :: CheckFailureReason
pattern CheckFailureReason_ASSUME_ROLE_ERROR = CheckFailureReason' "ASSUME_ROLE_ERROR"

pattern CheckFailureReason_PREMIUM_SUPPORT_REQUIRED :: CheckFailureReason
pattern CheckFailureReason_PREMIUM_SUPPORT_REQUIRED = CheckFailureReason' "PREMIUM_SUPPORT_REQUIRED"

pattern CheckFailureReason_UNKNOWN_ERROR :: CheckFailureReason
pattern CheckFailureReason_UNKNOWN_ERROR = CheckFailureReason' "UNKNOWN_ERROR"

{-# COMPLETE
  CheckFailureReason_ACCESS_DENIED,
  CheckFailureReason_ASSUME_ROLE_ERROR,
  CheckFailureReason_PREMIUM_SUPPORT_REQUIRED,
  CheckFailureReason_UNKNOWN_ERROR,
  CheckFailureReason'
  #-}
