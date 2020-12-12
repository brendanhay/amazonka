{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Eac3AtmosCodingMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Eac3AtmosCodingMode
  ( Eac3AtmosCodingMode
      ( Eac3AtmosCodingMode',
        CodingMode916
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | The coding mode for Dolby Digital Plus JOC (Atmos) is always 9.1.6 (CODING_MODE_9_1_6).
newtype Eac3AtmosCodingMode = Eac3AtmosCodingMode' Lude.Text
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

pattern CodingMode916 :: Eac3AtmosCodingMode
pattern CodingMode916 = Eac3AtmosCodingMode' "CODING_MODE_9_1_6"

{-# COMPLETE
  CodingMode916,
  Eac3AtmosCodingMode'
  #-}
