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
-- Module      : Amazonka.SSM.Types.ComplianceSeverity
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.ComplianceSeverity
  ( ComplianceSeverity
      ( ..,
        ComplianceSeverity_CRITICAL,
        ComplianceSeverity_HIGH,
        ComplianceSeverity_INFORMATIONAL,
        ComplianceSeverity_LOW,
        ComplianceSeverity_MEDIUM,
        ComplianceSeverity_UNSPECIFIED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype ComplianceSeverity = ComplianceSeverity'
  { fromComplianceSeverity ::
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

pattern ComplianceSeverity_CRITICAL :: ComplianceSeverity
pattern ComplianceSeverity_CRITICAL = ComplianceSeverity' "CRITICAL"

pattern ComplianceSeverity_HIGH :: ComplianceSeverity
pattern ComplianceSeverity_HIGH = ComplianceSeverity' "HIGH"

pattern ComplianceSeverity_INFORMATIONAL :: ComplianceSeverity
pattern ComplianceSeverity_INFORMATIONAL = ComplianceSeverity' "INFORMATIONAL"

pattern ComplianceSeverity_LOW :: ComplianceSeverity
pattern ComplianceSeverity_LOW = ComplianceSeverity' "LOW"

pattern ComplianceSeverity_MEDIUM :: ComplianceSeverity
pattern ComplianceSeverity_MEDIUM = ComplianceSeverity' "MEDIUM"

pattern ComplianceSeverity_UNSPECIFIED :: ComplianceSeverity
pattern ComplianceSeverity_UNSPECIFIED = ComplianceSeverity' "UNSPECIFIED"

{-# COMPLETE
  ComplianceSeverity_CRITICAL,
  ComplianceSeverity_HIGH,
  ComplianceSeverity_INFORMATIONAL,
  ComplianceSeverity_LOW,
  ComplianceSeverity_MEDIUM,
  ComplianceSeverity_UNSPECIFIED,
  ComplianceSeverity'
  #-}
