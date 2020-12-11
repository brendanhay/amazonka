-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.InputSourceEndBehavior
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.InputSourceEndBehavior
  ( InputSourceEndBehavior
      ( InputSourceEndBehavior',
        Continue,
        Loop
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Input Source End Behavior
newtype InputSourceEndBehavior = InputSourceEndBehavior' Lude.Text
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

pattern Continue :: InputSourceEndBehavior
pattern Continue = InputSourceEndBehavior' "CONTINUE"

pattern Loop :: InputSourceEndBehavior
pattern Loop = InputSourceEndBehavior' "LOOP"

{-# COMPLETE
  Continue,
  Loop,
  InputSourceEndBehavior'
  #-}
