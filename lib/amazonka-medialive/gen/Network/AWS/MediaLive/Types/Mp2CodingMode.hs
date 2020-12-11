-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Mp2CodingMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Mp2CodingMode
  ( Mp2CodingMode
      ( Mp2CodingMode',
        MCMCodingMode10,
        MCMCodingMode20
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Mp2 Coding Mode
newtype Mp2CodingMode = Mp2CodingMode' Lude.Text
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

pattern MCMCodingMode10 :: Mp2CodingMode
pattern MCMCodingMode10 = Mp2CodingMode' "CODING_MODE_1_0"

pattern MCMCodingMode20 :: Mp2CodingMode
pattern MCMCodingMode20 = Mp2CodingMode' "CODING_MODE_2_0"

{-# COMPLETE
  MCMCodingMode10,
  MCMCodingMode20,
  Mp2CodingMode'
  #-}
