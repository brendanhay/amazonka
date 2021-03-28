{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.M2tsEsRateInPes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.M2tsEsRateInPes
  ( M2tsEsRateInPes
    ( M2tsEsRateInPes'
    , M2tsEsRateInPesInclude
    , M2tsEsRateInPesExclude
    , fromM2tsEsRateInPes
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | Controls whether to include the ES Rate field in the PES header.
newtype M2tsEsRateInPes = M2tsEsRateInPes'{fromM2tsEsRateInPes ::
                                           Core.Text}
                            deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                            Core.Generic)
                            deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                              Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                              Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                              Core.FromText, Core.ToByteString, Core.ToQuery,
                                              Core.ToHeader)

pattern M2tsEsRateInPesInclude :: M2tsEsRateInPes
pattern M2tsEsRateInPesInclude = M2tsEsRateInPes' "INCLUDE"

pattern M2tsEsRateInPesExclude :: M2tsEsRateInPes
pattern M2tsEsRateInPesExclude = M2tsEsRateInPes' "EXCLUDE"

{-# COMPLETE 
  M2tsEsRateInPesInclude,

  M2tsEsRateInPesExclude,
  M2tsEsRateInPes'
  #-}
