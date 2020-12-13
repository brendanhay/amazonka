{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.PatchComplianceLevel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.PatchComplianceLevel
  ( PatchComplianceLevel
      ( PatchComplianceLevel',
        PCLCritical,
        PCLHigh,
        PCLMedium,
        PCLLow,
        PCLInformational,
        PCLUnspecified
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype PatchComplianceLevel = PatchComplianceLevel' Lude.Text
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

pattern PCLCritical :: PatchComplianceLevel
pattern PCLCritical = PatchComplianceLevel' "CRITICAL"

pattern PCLHigh :: PatchComplianceLevel
pattern PCLHigh = PatchComplianceLevel' "HIGH"

pattern PCLMedium :: PatchComplianceLevel
pattern PCLMedium = PatchComplianceLevel' "MEDIUM"

pattern PCLLow :: PatchComplianceLevel
pattern PCLLow = PatchComplianceLevel' "LOW"

pattern PCLInformational :: PatchComplianceLevel
pattern PCLInformational = PatchComplianceLevel' "INFORMATIONAL"

pattern PCLUnspecified :: PatchComplianceLevel
pattern PCLUnspecified = PatchComplianceLevel' "UNSPECIFIED"

{-# COMPLETE
  PCLCritical,
  PCLHigh,
  PCLMedium,
  PCLLow,
  PCLInformational,
  PCLUnspecified,
  PatchComplianceLevel'
  #-}
