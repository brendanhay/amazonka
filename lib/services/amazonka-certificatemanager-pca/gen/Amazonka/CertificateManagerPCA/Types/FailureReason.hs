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
-- Module      : Amazonka.CertificateManagerPCA.Types.FailureReason
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CertificateManagerPCA.Types.FailureReason
  ( FailureReason
      ( ..,
        FailureReason_OTHER,
        FailureReason_REQUEST_TIMED_OUT,
        FailureReason_UNSUPPORTED_ALGORITHM
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

pattern FailureReason_OTHER :: FailureReason
pattern FailureReason_OTHER = FailureReason' "OTHER"

pattern FailureReason_REQUEST_TIMED_OUT :: FailureReason
pattern FailureReason_REQUEST_TIMED_OUT = FailureReason' "REQUEST_TIMED_OUT"

pattern FailureReason_UNSUPPORTED_ALGORITHM :: FailureReason
pattern FailureReason_UNSUPPORTED_ALGORITHM = FailureReason' "UNSUPPORTED_ALGORITHM"

{-# COMPLETE
  FailureReason_OTHER,
  FailureReason_REQUEST_TIMED_OUT,
  FailureReason_UNSUPPORTED_ALGORITHM,
  FailureReason'
  #-}
