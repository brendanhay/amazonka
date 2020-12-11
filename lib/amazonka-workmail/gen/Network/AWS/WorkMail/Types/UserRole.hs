-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkMail.Types.UserRole
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkMail.Types.UserRole
  ( UserRole
      ( UserRole',
        URResource,
        URSystemUser,
        URUser
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype UserRole = UserRole' Lude.Text
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

pattern URResource :: UserRole
pattern URResource = UserRole' "RESOURCE"

pattern URSystemUser :: UserRole
pattern URSystemUser = UserRole' "SYSTEM_USER"

pattern URUser :: UserRole
pattern URUser = UserRole' "USER"

{-# COMPLETE
  URResource,
  URSystemUser,
  URUser,
  UserRole'
  #-}
