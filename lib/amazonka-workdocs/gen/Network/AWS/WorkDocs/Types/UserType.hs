{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.Types.UserType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.WorkDocs.Types.UserType
  ( UserType
    ( UserType'
    , UserTypeUser
    , UserTypeAdmin
    , UserTypePoweruser
    , UserTypeMinimaluser
    , UserTypeWorkspacesuser
    , fromUserType
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype UserType = UserType'{fromUserType :: Core.Text}
                     deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                     Core.Generic)
                     deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                       Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON, Core.FromJSON,
                                       Core.ToXML, Core.FromXML, Core.ToText, Core.FromText,
                                       Core.ToByteString, Core.ToQuery, Core.ToHeader)

pattern UserTypeUser :: UserType
pattern UserTypeUser = UserType' "USER"

pattern UserTypeAdmin :: UserType
pattern UserTypeAdmin = UserType' "ADMIN"

pattern UserTypePoweruser :: UserType
pattern UserTypePoweruser = UserType' "POWERUSER"

pattern UserTypeMinimaluser :: UserType
pattern UserTypeMinimaluser = UserType' "MINIMALUSER"

pattern UserTypeWorkspacesuser :: UserType
pattern UserTypeWorkspacesuser = UserType' "WORKSPACESUSER"

{-# COMPLETE 
  UserTypeUser,

  UserTypeAdmin,

  UserTypePoweruser,

  UserTypeMinimaluser,

  UserTypeWorkspacesuser,
  UserType'
  #-}
