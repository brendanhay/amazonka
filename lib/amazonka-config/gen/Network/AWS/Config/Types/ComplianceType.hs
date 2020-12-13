{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.ComplianceType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.ComplianceType
  ( ComplianceType
      ( ComplianceType',
        Compliant,
        NonCompliant,
        NotApplicable,
        InsufficientData
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ComplianceType = ComplianceType' Lude.Text
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

pattern Compliant :: ComplianceType
pattern Compliant = ComplianceType' "COMPLIANT"

pattern NonCompliant :: ComplianceType
pattern NonCompliant = ComplianceType' "NON_COMPLIANT"

pattern NotApplicable :: ComplianceType
pattern NotApplicable = ComplianceType' "NOT_APPLICABLE"

pattern InsufficientData :: ComplianceType
pattern InsufficientData = ComplianceType' "INSUFFICIENT_DATA"

{-# COMPLETE
  Compliant,
  NonCompliant,
  NotApplicable,
  InsufficientData,
  ComplianceType'
  #-}
