{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Cloud9.Types.EnvironmentStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Cloud9.Types.EnvironmentStatus
  ( EnvironmentStatus
      ( EnvironmentStatus',
        ESConnecting,
        ESCreating,
        ESDeleting,
        ESError,
        ESReady,
        ESStopped,
        ESStopping
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype EnvironmentStatus = EnvironmentStatus' Lude.Text
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

pattern ESConnecting :: EnvironmentStatus
pattern ESConnecting = EnvironmentStatus' "connecting"

pattern ESCreating :: EnvironmentStatus
pattern ESCreating = EnvironmentStatus' "creating"

pattern ESDeleting :: EnvironmentStatus
pattern ESDeleting = EnvironmentStatus' "deleting"

pattern ESError :: EnvironmentStatus
pattern ESError = EnvironmentStatus' "error"

pattern ESReady :: EnvironmentStatus
pattern ESReady = EnvironmentStatus' "ready"

pattern ESStopped :: EnvironmentStatus
pattern ESStopped = EnvironmentStatus' "stopped"

pattern ESStopping :: EnvironmentStatus
pattern ESStopping = EnvironmentStatus' "stopping"

{-# COMPLETE
  ESConnecting,
  ESCreating,
  ESDeleting,
  ESError,
  ESReady,
  ESStopped,
  ESStopping,
  EnvironmentStatus'
  #-}
