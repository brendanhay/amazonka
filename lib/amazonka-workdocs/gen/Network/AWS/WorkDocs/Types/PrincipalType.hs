{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.Types.PrincipalType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkDocs.Types.PrincipalType
  ( PrincipalType
      ( PrincipalType',
        User,
        Group,
        Invite,
        Anonymous,
        Organization
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype PrincipalType = PrincipalType' Lude.Text
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

pattern User :: PrincipalType
pattern User = PrincipalType' "USER"

pattern Group :: PrincipalType
pattern Group = PrincipalType' "GROUP"

pattern Invite :: PrincipalType
pattern Invite = PrincipalType' "INVITE"

pattern Anonymous :: PrincipalType
pattern Anonymous = PrincipalType' "ANONYMOUS"

pattern Organization :: PrincipalType
pattern Organization = PrincipalType' "ORGANIZATION"

{-# COMPLETE
  User,
  Group,
  Invite,
  Anonymous,
  Organization,
  PrincipalType'
  #-}
