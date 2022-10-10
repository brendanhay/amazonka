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
-- Module      : Amazonka.SecurityHub.Types.ComplianceStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.ComplianceStatus
  ( ComplianceStatus
      ( ..,
        ComplianceStatus_FAILED,
        ComplianceStatus_NOT_AVAILABLE,
        ComplianceStatus_PASSED,
        ComplianceStatus_WARNING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype ComplianceStatus = ComplianceStatus'
  { fromComplianceStatus ::
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

pattern ComplianceStatus_FAILED :: ComplianceStatus
pattern ComplianceStatus_FAILED = ComplianceStatus' "FAILED"

pattern ComplianceStatus_NOT_AVAILABLE :: ComplianceStatus
pattern ComplianceStatus_NOT_AVAILABLE = ComplianceStatus' "NOT_AVAILABLE"

pattern ComplianceStatus_PASSED :: ComplianceStatus
pattern ComplianceStatus_PASSED = ComplianceStatus' "PASSED"

pattern ComplianceStatus_WARNING :: ComplianceStatus
pattern ComplianceStatus_WARNING = ComplianceStatus' "WARNING"

{-# COMPLETE
  ComplianceStatus_FAILED,
  ComplianceStatus_NOT_AVAILABLE,
  ComplianceStatus_PASSED,
  ComplianceStatus_WARNING,
  ComplianceStatus'
  #-}
