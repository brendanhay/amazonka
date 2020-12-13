{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.Types.KeyState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KMS.Types.KeyState
  ( KeyState
      ( KeyState',
        Enabled,
        Disabled,
        PendingDeletion,
        PendingImport,
        Unavailable
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype KeyState = KeyState' Lude.Text
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

pattern Enabled :: KeyState
pattern Enabled = KeyState' "Enabled"

pattern Disabled :: KeyState
pattern Disabled = KeyState' "Disabled"

pattern PendingDeletion :: KeyState
pattern PendingDeletion = KeyState' "PendingDeletion"

pattern PendingImport :: KeyState
pattern PendingImport = KeyState' "PendingImport"

pattern Unavailable :: KeyState
pattern Unavailable = KeyState' "Unavailable"

{-# COMPLETE
  Enabled,
  Disabled,
  PendingDeletion,
  PendingImport,
  Unavailable,
  KeyState'
  #-}
