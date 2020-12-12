{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Eac3SurroundExMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Eac3SurroundExMode
  ( Eac3SurroundExMode
      ( Eac3SurroundExMode',
        ESEMDisabled,
        ESEMEnabled,
        ESEMNotIndicated
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | When encoding 3/2 audio, sets whether an extra center back surround channel is matrix encoded into the left and right surround channels.
newtype Eac3SurroundExMode = Eac3SurroundExMode' Lude.Text
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

pattern ESEMDisabled :: Eac3SurroundExMode
pattern ESEMDisabled = Eac3SurroundExMode' "DISABLED"

pattern ESEMEnabled :: Eac3SurroundExMode
pattern ESEMEnabled = Eac3SurroundExMode' "ENABLED"

pattern ESEMNotIndicated :: Eac3SurroundExMode
pattern ESEMNotIndicated = Eac3SurroundExMode' "NOT_INDICATED"

{-# COMPLETE
  ESEMDisabled,
  ESEMEnabled,
  ESEMNotIndicated,
  Eac3SurroundExMode'
  #-}
