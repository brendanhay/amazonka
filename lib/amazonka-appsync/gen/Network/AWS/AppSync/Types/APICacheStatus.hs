{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.Types.APICacheStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppSync.Types.APICacheStatus
  ( APICacheStatus
      ( APICacheStatus',
        Available,
        Creating,
        Deleting,
        Failed,
        Modifying
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype APICacheStatus = APICacheStatus' Lude.Text
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

pattern Available :: APICacheStatus
pattern Available = APICacheStatus' "AVAILABLE"

pattern Creating :: APICacheStatus
pattern Creating = APICacheStatus' "CREATING"

pattern Deleting :: APICacheStatus
pattern Deleting = APICacheStatus' "DELETING"

pattern Failed :: APICacheStatus
pattern Failed = APICacheStatus' "FAILED"

pattern Modifying :: APICacheStatus
pattern Modifying = APICacheStatus' "MODIFYING"

{-# COMPLETE
  Available,
  Creating,
  Deleting,
  Failed,
  Modifying,
  APICacheStatus'
  #-}
