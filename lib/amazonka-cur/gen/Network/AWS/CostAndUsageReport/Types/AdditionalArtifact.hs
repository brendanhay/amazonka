{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostAndUsageReport.Types.AdditionalArtifact
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostAndUsageReport.Types.AdditionalArtifact
  ( AdditionalArtifact
      ( AdditionalArtifact',
        AdditionalArtifactRedshift,
        AdditionalArtifactQuicksight,
        AdditionalArtifactAthena,
        fromAdditionalArtifact
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | The types of manifest that you want AWS to create for this report.
newtype AdditionalArtifact = AdditionalArtifact'
  { fromAdditionalArtifact ::
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

pattern AdditionalArtifactRedshift :: AdditionalArtifact
pattern AdditionalArtifactRedshift = AdditionalArtifact' "REDSHIFT"

pattern AdditionalArtifactQuicksight :: AdditionalArtifact
pattern AdditionalArtifactQuicksight = AdditionalArtifact' "QUICKSIGHT"

pattern AdditionalArtifactAthena :: AdditionalArtifact
pattern AdditionalArtifactAthena = AdditionalArtifact' "ATHENA"

{-# COMPLETE
  AdditionalArtifactRedshift,
  AdditionalArtifactQuicksight,
  AdditionalArtifactAthena,
  AdditionalArtifact'
  #-}
