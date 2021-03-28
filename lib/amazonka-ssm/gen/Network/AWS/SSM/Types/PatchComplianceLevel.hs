{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.PatchComplianceLevel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.PatchComplianceLevel
  ( PatchComplianceLevel
    ( PatchComplianceLevel'
    , PatchComplianceLevelCritical
    , PatchComplianceLevelHigh
    , PatchComplianceLevelMedium
    , PatchComplianceLevelLow
    , PatchComplianceLevelInformational
    , PatchComplianceLevelUnspecified
    , fromPatchComplianceLevel
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype PatchComplianceLevel = PatchComplianceLevel'{fromPatchComplianceLevel
                                                     :: Core.Text}
                                 deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                 Core.Generic)
                                 deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                   Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                   Core.FromJSON, Core.ToXML, Core.FromXML,
                                                   Core.ToText, Core.FromText, Core.ToByteString,
                                                   Core.ToQuery, Core.ToHeader)

pattern PatchComplianceLevelCritical :: PatchComplianceLevel
pattern PatchComplianceLevelCritical = PatchComplianceLevel' "CRITICAL"

pattern PatchComplianceLevelHigh :: PatchComplianceLevel
pattern PatchComplianceLevelHigh = PatchComplianceLevel' "HIGH"

pattern PatchComplianceLevelMedium :: PatchComplianceLevel
pattern PatchComplianceLevelMedium = PatchComplianceLevel' "MEDIUM"

pattern PatchComplianceLevelLow :: PatchComplianceLevel
pattern PatchComplianceLevelLow = PatchComplianceLevel' "LOW"

pattern PatchComplianceLevelInformational :: PatchComplianceLevel
pattern PatchComplianceLevelInformational = PatchComplianceLevel' "INFORMATIONAL"

pattern PatchComplianceLevelUnspecified :: PatchComplianceLevel
pattern PatchComplianceLevelUnspecified = PatchComplianceLevel' "UNSPECIFIED"

{-# COMPLETE 
  PatchComplianceLevelCritical,

  PatchComplianceLevelHigh,

  PatchComplianceLevelMedium,

  PatchComplianceLevelLow,

  PatchComplianceLevelInformational,

  PatchComplianceLevelUnspecified,
  PatchComplianceLevel'
  #-}
