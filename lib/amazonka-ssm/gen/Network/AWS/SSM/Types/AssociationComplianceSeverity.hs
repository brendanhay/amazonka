-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.AssociationComplianceSeverity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.AssociationComplianceSeverity
  ( AssociationComplianceSeverity
      ( AssociationComplianceSeverity',
        ACSCritical,
        ACSHigh,
        ACSLow,
        ACSMedium,
        ACSUnspecified
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype AssociationComplianceSeverity = AssociationComplianceSeverity' Lude.Text
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

pattern ACSCritical :: AssociationComplianceSeverity
pattern ACSCritical = AssociationComplianceSeverity' "CRITICAL"

pattern ACSHigh :: AssociationComplianceSeverity
pattern ACSHigh = AssociationComplianceSeverity' "HIGH"

pattern ACSLow :: AssociationComplianceSeverity
pattern ACSLow = AssociationComplianceSeverity' "LOW"

pattern ACSMedium :: AssociationComplianceSeverity
pattern ACSMedium = AssociationComplianceSeverity' "MEDIUM"

pattern ACSUnspecified :: AssociationComplianceSeverity
pattern ACSUnspecified = AssociationComplianceSeverity' "UNSPECIFIED"

{-# COMPLETE
  ACSCritical,
  ACSHigh,
  ACSLow,
  ACSMedium,
  ACSUnspecified,
  AssociationComplianceSeverity'
  #-}
