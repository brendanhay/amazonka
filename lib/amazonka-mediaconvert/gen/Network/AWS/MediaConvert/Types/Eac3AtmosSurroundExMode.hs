{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Eac3AtmosSurroundExMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Eac3AtmosSurroundExMode
  ( Eac3AtmosSurroundExMode
      ( Eac3AtmosSurroundExMode',
        EASEMNotIndicated,
        EASEMEnabled,
        EASEMDisabled
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Specify whether your input audio has an additional center rear surround channel matrix encoded into your left and right surround channels.
newtype Eac3AtmosSurroundExMode = Eac3AtmosSurroundExMode' Lude.Text
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

pattern EASEMNotIndicated :: Eac3AtmosSurroundExMode
pattern EASEMNotIndicated = Eac3AtmosSurroundExMode' "NOT_INDICATED"

pattern EASEMEnabled :: Eac3AtmosSurroundExMode
pattern EASEMEnabled = Eac3AtmosSurroundExMode' "ENABLED"

pattern EASEMDisabled :: Eac3AtmosSurroundExMode
pattern EASEMDisabled = Eac3AtmosSurroundExMode' "DISABLED"

{-# COMPLETE
  EASEMNotIndicated,
  EASEMEnabled,
  EASEMDisabled,
  Eac3AtmosSurroundExMode'
  #-}
