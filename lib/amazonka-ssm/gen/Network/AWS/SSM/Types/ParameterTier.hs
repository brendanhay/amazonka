{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.ParameterTier
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.ParameterTier
  ( ParameterTier
      ( ParameterTier',
        ParameterTierStandard,
        ParameterTierAdvanced,
        ParameterTierIntelligentTiering,
        fromParameterTier
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype ParameterTier = ParameterTier'
  { fromParameterTier ::
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

pattern ParameterTierStandard :: ParameterTier
pattern ParameterTierStandard = ParameterTier' "Standard"

pattern ParameterTierAdvanced :: ParameterTier
pattern ParameterTierAdvanced = ParameterTier' "Advanced"

pattern ParameterTierIntelligentTiering :: ParameterTier
pattern ParameterTierIntelligentTiering = ParameterTier' "Intelligent-Tiering"

{-# COMPLETE
  ParameterTierStandard,
  ParameterTierAdvanced,
  ParameterTierIntelligentTiering,
  ParameterTier'
  #-}
