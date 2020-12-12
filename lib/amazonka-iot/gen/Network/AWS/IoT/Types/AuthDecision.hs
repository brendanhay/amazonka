{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.AuthDecision
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.AuthDecision
  ( AuthDecision
      ( AuthDecision',
        Allowed,
        ExplicitDeny,
        ImplicitDeny
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype AuthDecision = AuthDecision' Lude.Text
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

pattern Allowed :: AuthDecision
pattern Allowed = AuthDecision' "ALLOWED"

pattern ExplicitDeny :: AuthDecision
pattern ExplicitDeny = AuthDecision' "EXPLICIT_DENY"

pattern ImplicitDeny :: AuthDecision
pattern ImplicitDeny = AuthDecision' "IMPLICIT_DENY"

{-# COMPLETE
  Allowed,
  ExplicitDeny,
  ImplicitDeny,
  AuthDecision'
  #-}
