{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.FixedAfd
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.FixedAfd
  ( FixedAfd
      ( FixedAfd',
        Afd0000,
        Afd0010,
        Afd0011,
        Afd0100,
        Afd1000,
        Afd1001,
        Afd1010,
        Afd1011,
        Afd1101,
        Afd1110,
        Afd1111
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Fixed Afd
newtype FixedAfd = FixedAfd' Lude.Text
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

pattern Afd0000 :: FixedAfd
pattern Afd0000 = FixedAfd' "AFD_0000"

pattern Afd0010 :: FixedAfd
pattern Afd0010 = FixedAfd' "AFD_0010"

pattern Afd0011 :: FixedAfd
pattern Afd0011 = FixedAfd' "AFD_0011"

pattern Afd0100 :: FixedAfd
pattern Afd0100 = FixedAfd' "AFD_0100"

pattern Afd1000 :: FixedAfd
pattern Afd1000 = FixedAfd' "AFD_1000"

pattern Afd1001 :: FixedAfd
pattern Afd1001 = FixedAfd' "AFD_1001"

pattern Afd1010 :: FixedAfd
pattern Afd1010 = FixedAfd' "AFD_1010"

pattern Afd1011 :: FixedAfd
pattern Afd1011 = FixedAfd' "AFD_1011"

pattern Afd1101 :: FixedAfd
pattern Afd1101 = FixedAfd' "AFD_1101"

pattern Afd1110 :: FixedAfd
pattern Afd1110 = FixedAfd' "AFD_1110"

pattern Afd1111 :: FixedAfd
pattern Afd1111 = FixedAfd' "AFD_1111"

{-# COMPLETE
  Afd0000,
  Afd0010,
  Afd0011,
  Afd0100,
  Afd1000,
  Afd1001,
  Afd1010,
  Afd1011,
  Afd1101,
  Afd1110,
  Afd1111,
  FixedAfd'
  #-}
