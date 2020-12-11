-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Eac3PassthroughControl
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Eac3PassthroughControl
  ( Eac3PassthroughControl
      ( Eac3PassthroughControl',
        NoPassthrough,
        WhenPossible
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Eac3 Passthrough Control
newtype Eac3PassthroughControl = Eac3PassthroughControl' Lude.Text
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

pattern NoPassthrough :: Eac3PassthroughControl
pattern NoPassthrough = Eac3PassthroughControl' "NO_PASSTHROUGH"

pattern WhenPossible :: Eac3PassthroughControl
pattern WhenPossible = Eac3PassthroughControl' "WHEN_POSSIBLE"

{-# COMPLETE
  NoPassthrough,
  WhenPossible,
  Eac3PassthroughControl'
  #-}
