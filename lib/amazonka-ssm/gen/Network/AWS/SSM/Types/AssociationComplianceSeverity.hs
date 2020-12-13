{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
        Critical,
        High,
        Medium,
        Low,
        Unspecified
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

pattern Critical :: AssociationComplianceSeverity
pattern Critical = AssociationComplianceSeverity' "CRITICAL"

pattern High :: AssociationComplianceSeverity
pattern High = AssociationComplianceSeverity' "HIGH"

pattern Medium :: AssociationComplianceSeverity
pattern Medium = AssociationComplianceSeverity' "MEDIUM"

pattern Low :: AssociationComplianceSeverity
pattern Low = AssociationComplianceSeverity' "LOW"

pattern Unspecified :: AssociationComplianceSeverity
pattern Unspecified = AssociationComplianceSeverity' "UNSPECIFIED"

{-# COMPLETE
  Critical,
  High,
  Medium,
  Low,
  Unspecified,
  AssociationComplianceSeverity'
  #-}
