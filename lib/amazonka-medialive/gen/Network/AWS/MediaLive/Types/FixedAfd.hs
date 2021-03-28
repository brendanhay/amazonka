{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.FixedAfd
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.FixedAfd
  ( FixedAfd
    ( FixedAfd'
    , FixedAfdAfd0000
    , FixedAfdAfd0010
    , FixedAfdAfd0011
    , FixedAfdAfd0100
    , FixedAfdAfd1000
    , FixedAfdAfd1001
    , FixedAfdAfd1010
    , FixedAfdAfd1011
    , FixedAfdAfd1101
    , FixedAfdAfd1110
    , FixedAfdAfd1111
    , fromFixedAfd
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | Fixed Afd
newtype FixedAfd = FixedAfd'{fromFixedAfd :: Core.Text}
                     deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                     Core.Generic)
                     deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                       Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON, Core.FromJSON,
                                       Core.ToXML, Core.FromXML, Core.ToText, Core.FromText,
                                       Core.ToByteString, Core.ToQuery, Core.ToHeader)

pattern FixedAfdAfd0000 :: FixedAfd
pattern FixedAfdAfd0000 = FixedAfd' "AFD_0000"

pattern FixedAfdAfd0010 :: FixedAfd
pattern FixedAfdAfd0010 = FixedAfd' "AFD_0010"

pattern FixedAfdAfd0011 :: FixedAfd
pattern FixedAfdAfd0011 = FixedAfd' "AFD_0011"

pattern FixedAfdAfd0100 :: FixedAfd
pattern FixedAfdAfd0100 = FixedAfd' "AFD_0100"

pattern FixedAfdAfd1000 :: FixedAfd
pattern FixedAfdAfd1000 = FixedAfd' "AFD_1000"

pattern FixedAfdAfd1001 :: FixedAfd
pattern FixedAfdAfd1001 = FixedAfd' "AFD_1001"

pattern FixedAfdAfd1010 :: FixedAfd
pattern FixedAfdAfd1010 = FixedAfd' "AFD_1010"

pattern FixedAfdAfd1011 :: FixedAfd
pattern FixedAfdAfd1011 = FixedAfd' "AFD_1011"

pattern FixedAfdAfd1101 :: FixedAfd
pattern FixedAfdAfd1101 = FixedAfd' "AFD_1101"

pattern FixedAfdAfd1110 :: FixedAfd
pattern FixedAfdAfd1110 = FixedAfd' "AFD_1110"

pattern FixedAfdAfd1111 :: FixedAfd
pattern FixedAfdAfd1111 = FixedAfd' "AFD_1111"

{-# COMPLETE 
  FixedAfdAfd0000,

  FixedAfdAfd0010,

  FixedAfdAfd0011,

  FixedAfdAfd0100,

  FixedAfdAfd1000,

  FixedAfdAfd1001,

  FixedAfdAfd1010,

  FixedAfdAfd1011,

  FixedAfdAfd1101,

  FixedAfdAfd1110,

  FixedAfdAfd1111,
  FixedAfd'
  #-}
