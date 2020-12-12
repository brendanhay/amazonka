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
        Critical,
        High,
        Informational,
        Low,
        Medium,
        Unspecified
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

pattern Critical :: PatchComplianceLevel
pattern Critical = PatchComplianceLevel' "CRITICAL"

pattern High :: PatchComplianceLevel
pattern High = PatchComplianceLevel' "HIGH"

pattern Informational :: PatchComplianceLevel
pattern Informational = PatchComplianceLevel' "INFORMATIONAL"

pattern Low :: PatchComplianceLevel
pattern Low = PatchComplianceLevel' "LOW"

pattern Medium :: PatchComplianceLevel
pattern Medium = PatchComplianceLevel' "MEDIUM"

pattern Unspecified :: PatchComplianceLevel
pattern Unspecified = PatchComplianceLevel' "UNSPECIFIED"

{-# COMPLETE
  Critical,
  High,
  Informational,
  Low,
  Medium,
  Unspecified,
  PatchComplianceLevel'
  #-}
