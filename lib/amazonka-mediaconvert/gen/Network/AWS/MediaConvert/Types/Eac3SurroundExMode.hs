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
        NotIndicated,
        Enabled,
        Disabled
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

pattern NotIndicated :: Eac3SurroundExMode
pattern NotIndicated = Eac3SurroundExMode' "NOT_INDICATED"

pattern Enabled :: Eac3SurroundExMode
pattern Enabled = Eac3SurroundExMode' "ENABLED"

pattern Disabled :: Eac3SurroundExMode
pattern Disabled = Eac3SurroundExMode' "DISABLED"

{-# COMPLETE
  NotIndicated,
  Enabled,
  Disabled,
  Eac3SurroundExMode'
  #-}
