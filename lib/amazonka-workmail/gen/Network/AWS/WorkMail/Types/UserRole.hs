{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
        UserRoleUser,
        UserRoleResource,
        UserRoleSystemUser,
        fromUserRole
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype UserRole = UserRole' {fromUserRole :: Core.Text}
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern UserRoleUser :: UserRole
pattern UserRoleUser = UserRole' "USER"

pattern UserRoleResource :: UserRole
pattern UserRoleResource = UserRole' "RESOURCE"

pattern UserRoleSystemUser :: UserRole
pattern UserRoleSystemUser = UserRole' "SYSTEM_USER"

{-# COMPLETE
  UserRoleUser,
  UserRoleResource,
  UserRoleSystemUser,
  UserRole'
  #-}
