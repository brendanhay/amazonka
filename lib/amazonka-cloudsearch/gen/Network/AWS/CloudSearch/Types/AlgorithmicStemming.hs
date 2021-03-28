{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.Types.AlgorithmicStemming
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudSearch.Types.AlgorithmicStemming
  ( AlgorithmicStemming
    ( AlgorithmicStemming'
    , AlgorithmicStemmingNone
    , AlgorithmicStemmingMinimal
    , AlgorithmicStemmingLight
    , AlgorithmicStemmingFull
    , fromAlgorithmicStemming
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype AlgorithmicStemming = AlgorithmicStemming'{fromAlgorithmicStemming
                                                   :: Core.Text}
                                deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                Core.Generic)
                                deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                  Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                  Core.FromJSON, Core.ToXML, Core.FromXML,
                                                  Core.ToText, Core.FromText, Core.ToByteString,
                                                  Core.ToQuery, Core.ToHeader)

pattern AlgorithmicStemmingNone :: AlgorithmicStemming
pattern AlgorithmicStemmingNone = AlgorithmicStemming' "none"

pattern AlgorithmicStemmingMinimal :: AlgorithmicStemming
pattern AlgorithmicStemmingMinimal = AlgorithmicStemming' "minimal"

pattern AlgorithmicStemmingLight :: AlgorithmicStemming
pattern AlgorithmicStemmingLight = AlgorithmicStemming' "light"

pattern AlgorithmicStemmingFull :: AlgorithmicStemming
pattern AlgorithmicStemmingFull = AlgorithmicStemming' "full"

{-# COMPLETE 
  AlgorithmicStemmingNone,

  AlgorithmicStemmingMinimal,

  AlgorithmicStemmingLight,

  AlgorithmicStemmingFull,
  AlgorithmicStemming'
  #-}
