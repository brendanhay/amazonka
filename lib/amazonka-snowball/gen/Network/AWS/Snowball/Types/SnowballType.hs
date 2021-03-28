{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Snowball.Types.SnowballType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Snowball.Types.SnowballType
  ( SnowballType
    ( SnowballType'
    , SnowballTypeStandard
    , SnowballTypeEdge
    , SnowballTypeEdgeC
    , SnowballTypeEdgeCg
    , SnowballTypeEdgeS
    , SnowballTypeSNC1Hdd
    , fromSnowballType
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype SnowballType = SnowballType'{fromSnowballType :: Core.Text}
                         deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                         Core.Generic)
                         deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                           Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                           Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                           Core.FromText, Core.ToByteString, Core.ToQuery,
                                           Core.ToHeader)

pattern SnowballTypeStandard :: SnowballType
pattern SnowballTypeStandard = SnowballType' "STANDARD"

pattern SnowballTypeEdge :: SnowballType
pattern SnowballTypeEdge = SnowballType' "EDGE"

pattern SnowballTypeEdgeC :: SnowballType
pattern SnowballTypeEdgeC = SnowballType' "EDGE_C"

pattern SnowballTypeEdgeCg :: SnowballType
pattern SnowballTypeEdgeCg = SnowballType' "EDGE_CG"

pattern SnowballTypeEdgeS :: SnowballType
pattern SnowballTypeEdgeS = SnowballType' "EDGE_S"

pattern SnowballTypeSNC1Hdd :: SnowballType
pattern SnowballTypeSNC1Hdd = SnowballType' "SNC1_HDD"

{-# COMPLETE 
  SnowballTypeStandard,

  SnowballTypeEdge,

  SnowballTypeEdgeC,

  SnowballTypeEdgeCg,

  SnowballTypeEdgeS,

  SnowballTypeSNC1Hdd,
  SnowballType'
  #-}
