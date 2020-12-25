{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.ComplianceSeverity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.ComplianceSeverity
  ( ComplianceSeverity
      ( ComplianceSeverity',
        ComplianceSeverityCritical,
        ComplianceSeverityHigh,
        ComplianceSeverityMedium,
        ComplianceSeverityLow,
        ComplianceSeverityInformational,
        ComplianceSeverityUnspecified,
        fromComplianceSeverity
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype ComplianceSeverity = ComplianceSeverity'
  { fromComplianceSeverity ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern ComplianceSeverityCritical :: ComplianceSeverity
pattern ComplianceSeverityCritical = ComplianceSeverity' "CRITICAL"

pattern ComplianceSeverityHigh :: ComplianceSeverity
pattern ComplianceSeverityHigh = ComplianceSeverity' "HIGH"

pattern ComplianceSeverityMedium :: ComplianceSeverity
pattern ComplianceSeverityMedium = ComplianceSeverity' "MEDIUM"

pattern ComplianceSeverityLow :: ComplianceSeverity
pattern ComplianceSeverityLow = ComplianceSeverity' "LOW"

pattern ComplianceSeverityInformational :: ComplianceSeverity
pattern ComplianceSeverityInformational = ComplianceSeverity' "INFORMATIONAL"

pattern ComplianceSeverityUnspecified :: ComplianceSeverity
pattern ComplianceSeverityUnspecified = ComplianceSeverity' "UNSPECIFIED"

{-# COMPLETE
  ComplianceSeverityCritical,
  ComplianceSeverityHigh,
  ComplianceSeverityMedium,
  ComplianceSeverityLow,
  ComplianceSeverityInformational,
  ComplianceSeverityUnspecified,
  ComplianceSeverity'
  #-}
