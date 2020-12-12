{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.InputState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.InputState
  ( InputState
      ( InputState',
        Attached,
        Creating,
        Deleted,
        Deleting,
        Detached
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Placeholder documentation for InputState
newtype InputState = InputState' Lude.Text
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

pattern Attached :: InputState
pattern Attached = InputState' "ATTACHED"

pattern Creating :: InputState
pattern Creating = InputState' "CREATING"

pattern Deleted :: InputState
pattern Deleted = InputState' "DELETED"

pattern Deleting :: InputState
pattern Deleting = InputState' "DELETING"

pattern Detached :: InputState
pattern Detached = InputState' "DETACHED"

{-# COMPLETE
  Attached,
  Creating,
  Deleted,
  Deleting,
  Detached,
  InputState'
  #-}
