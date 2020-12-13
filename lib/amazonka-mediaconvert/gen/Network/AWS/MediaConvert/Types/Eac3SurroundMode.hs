{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Eac3SurroundMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Eac3SurroundMode
  ( Eac3SurroundMode
      ( Eac3SurroundMode',
        ESMNotIndicated,
        ESMEnabled,
        ESMDisabled
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | When encoding 2/0 audio, sets whether Dolby Surround is matrix encoded into the two channels.
newtype Eac3SurroundMode = Eac3SurroundMode' Lude.Text
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

pattern ESMNotIndicated :: Eac3SurroundMode
pattern ESMNotIndicated = Eac3SurroundMode' "NOT_INDICATED"

pattern ESMEnabled :: Eac3SurroundMode
pattern ESMEnabled = Eac3SurroundMode' "ENABLED"

pattern ESMDisabled :: Eac3SurroundMode
pattern ESMDisabled = Eac3SurroundMode' "DISABLED"

{-# COMPLETE
  ESMNotIndicated,
  ESMEnabled,
  ESMDisabled,
  Eac3SurroundMode'
  #-}
