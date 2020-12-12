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
        CSCritical,
        CSHigh,
        CSInformational,
        CSLow,
        CSMedium,
        CSUnspecified
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ComplianceSeverity = ComplianceSeverity' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern CSCritical :: ComplianceSeverity
pattern CSCritical = ComplianceSeverity' "CRITICAL"

pattern CSHigh :: ComplianceSeverity
pattern CSHigh = ComplianceSeverity' "HIGH"

pattern CSInformational :: ComplianceSeverity
pattern CSInformational = ComplianceSeverity' "INFORMATIONAL"

pattern CSLow :: ComplianceSeverity
pattern CSLow = ComplianceSeverity' "LOW"

pattern CSMedium :: ComplianceSeverity
pattern CSMedium = ComplianceSeverity' "MEDIUM"

pattern CSUnspecified :: ComplianceSeverity
pattern CSUnspecified = ComplianceSeverity' "UNSPECIFIED"

{-# COMPLETE
  CSCritical,
  CSHigh,
  CSInformational,
  CSLow,
  CSMedium,
  CSUnspecified,
  ComplianceSeverity'
  #-}
