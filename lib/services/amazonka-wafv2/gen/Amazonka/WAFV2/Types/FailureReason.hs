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
-- Module      : Amazonka.WAFV2.Types.FailureReason
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WAFV2.Types.FailureReason
  ( FailureReason
      ( ..,
        FailureReason_TOKEN_DOMAIN_MISMATCH,
        FailureReason_TOKEN_EXPIRED,
        FailureReason_TOKEN_INVALID,
        FailureReason_TOKEN_MISSING
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

pattern FailureReason_TOKEN_DOMAIN_MISMATCH :: FailureReason
pattern FailureReason_TOKEN_DOMAIN_MISMATCH = FailureReason' "TOKEN_DOMAIN_MISMATCH"

pattern FailureReason_TOKEN_EXPIRED :: FailureReason
pattern FailureReason_TOKEN_EXPIRED = FailureReason' "TOKEN_EXPIRED"

pattern FailureReason_TOKEN_INVALID :: FailureReason
pattern FailureReason_TOKEN_INVALID = FailureReason' "TOKEN_INVALID"

pattern FailureReason_TOKEN_MISSING :: FailureReason
pattern FailureReason_TOKEN_MISSING = FailureReason' "TOKEN_MISSING"

{-# COMPLETE
  FailureReason_TOKEN_DOMAIN_MISMATCH,
  FailureReason_TOKEN_EXPIRED,
  FailureReason_TOKEN_INVALID,
  FailureReason_TOKEN_MISSING,
  FailureReason'
  #-}
