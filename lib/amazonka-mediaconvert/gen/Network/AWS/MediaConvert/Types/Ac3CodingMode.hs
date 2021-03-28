{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Ac3CodingMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.Ac3CodingMode
  ( Ac3CodingMode
    ( Ac3CodingMode'
    , Ac3CodingModeCodingMode10
    , Ac3CodingModeCodingMode11
    , Ac3CodingModeCodingMode20
    , Ac3CodingModeCodingMode32Lfe
    , fromAc3CodingMode
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | Dolby Digital coding mode. Determines number of channels.
newtype Ac3CodingMode = Ac3CodingMode'{fromAc3CodingMode ::
                                       Core.Text}
                          deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                          Core.Generic)
                          deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                            Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                            Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                            Core.FromText, Core.ToByteString, Core.ToQuery,
                                            Core.ToHeader)

pattern Ac3CodingModeCodingMode10 :: Ac3CodingMode
pattern Ac3CodingModeCodingMode10 = Ac3CodingMode' "CODING_MODE_1_0"

pattern Ac3CodingModeCodingMode11 :: Ac3CodingMode
pattern Ac3CodingModeCodingMode11 = Ac3CodingMode' "CODING_MODE_1_1"

pattern Ac3CodingModeCodingMode20 :: Ac3CodingMode
pattern Ac3CodingModeCodingMode20 = Ac3CodingMode' "CODING_MODE_2_0"

pattern Ac3CodingModeCodingMode32Lfe :: Ac3CodingMode
pattern Ac3CodingModeCodingMode32Lfe = Ac3CodingMode' "CODING_MODE_3_2_LFE"

{-# COMPLETE 
  Ac3CodingModeCodingMode10,

  Ac3CodingModeCodingMode11,

  Ac3CodingModeCodingMode20,

  Ac3CodingModeCodingMode32Lfe,
  Ac3CodingMode'
  #-}
