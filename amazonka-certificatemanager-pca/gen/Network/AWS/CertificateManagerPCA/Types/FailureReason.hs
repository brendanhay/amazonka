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
-- Module      : Network.AWS.CertificateManagerPCA.Types.FailureReason
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CertificateManagerPCA.Types.FailureReason
  ( FailureReason
      ( ..,
        FailureReason_OTHER,
        FailureReason_REQUEST_TIMED_OUT,
        FailureReason_UNSUPPORTED_ALGORITHM
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
