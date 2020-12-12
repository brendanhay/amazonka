{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Eac3CodingMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Eac3CodingMode
  ( Eac3CodingMode
      ( Eac3CodingMode',
        ECMCodingMode10,
        ECMCodingMode20,
        ECMCodingMode32
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Dolby Digital Plus coding mode. Determines number of channels.
newtype Eac3CodingMode = Eac3CodingMode' Lude.Text
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

pattern ECMCodingMode10 :: Eac3CodingMode
pattern ECMCodingMode10 = Eac3CodingMode' "CODING_MODE_1_0"

pattern ECMCodingMode20 :: Eac3CodingMode
pattern ECMCodingMode20 = Eac3CodingMode' "CODING_MODE_2_0"

pattern ECMCodingMode32 :: Eac3CodingMode
pattern ECMCodingMode32 = Eac3CodingMode' "CODING_MODE_3_2"

{-# COMPLETE
  ECMCodingMode10,
  ECMCodingMode20,
  ECMCodingMode32,
  Eac3CodingMode'
  #-}
