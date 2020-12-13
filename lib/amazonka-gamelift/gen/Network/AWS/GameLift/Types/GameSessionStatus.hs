{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.GameSessionStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.GameSessionStatus
  ( GameSessionStatus
      ( GameSessionStatus',
        GSSActive,
        GSSActivating,
        GSSTerminated,
        GSSTerminating,
        GSSError
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype GameSessionStatus = GameSessionStatus' Lude.Text
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

pattern GSSActive :: GameSessionStatus
pattern GSSActive = GameSessionStatus' "ACTIVE"

pattern GSSActivating :: GameSessionStatus
pattern GSSActivating = GameSessionStatus' "ACTIVATING"

pattern GSSTerminated :: GameSessionStatus
pattern GSSTerminated = GameSessionStatus' "TERMINATED"

pattern GSSTerminating :: GameSessionStatus
pattern GSSTerminating = GameSessionStatus' "TERMINATING"

pattern GSSError :: GameSessionStatus
pattern GSSError = GameSessionStatus' "ERROR"

{-# COMPLETE
  GSSActive,
  GSSActivating,
  GSSTerminated,
  GSSTerminating,
  GSSError,
  GameSessionStatus'
  #-}
