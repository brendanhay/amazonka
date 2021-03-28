{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.Types.UserStackAssociationErrorCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AppStream.Types.UserStackAssociationErrorCode
  ( UserStackAssociationErrorCode
    ( UserStackAssociationErrorCode'
    , UserStackAssociationErrorCodeStackNotFound
    , UserStackAssociationErrorCodeUserNameNotFound
    , UserStackAssociationErrorCodeDirectoryNotFound
    , UserStackAssociationErrorCodeInternalError
    , fromUserStackAssociationErrorCode
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype UserStackAssociationErrorCode = UserStackAssociationErrorCode'{fromUserStackAssociationErrorCode
                                                                       :: Core.Text}
                                          deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                          Core.Generic)
                                          deriving newtype (Core.IsString, Core.Hashable,
                                                            Core.NFData, Core.ToJSONKey,
                                                            Core.FromJSONKey, Core.ToJSON,
                                                            Core.FromJSON, Core.ToXML, Core.FromXML,
                                                            Core.ToText, Core.FromText,
                                                            Core.ToByteString, Core.ToQuery,
                                                            Core.ToHeader)

pattern UserStackAssociationErrorCodeStackNotFound :: UserStackAssociationErrorCode
pattern UserStackAssociationErrorCodeStackNotFound = UserStackAssociationErrorCode' "STACK_NOT_FOUND"

pattern UserStackAssociationErrorCodeUserNameNotFound :: UserStackAssociationErrorCode
pattern UserStackAssociationErrorCodeUserNameNotFound = UserStackAssociationErrorCode' "USER_NAME_NOT_FOUND"

pattern UserStackAssociationErrorCodeDirectoryNotFound :: UserStackAssociationErrorCode
pattern UserStackAssociationErrorCodeDirectoryNotFound = UserStackAssociationErrorCode' "DIRECTORY_NOT_FOUND"

pattern UserStackAssociationErrorCodeInternalError :: UserStackAssociationErrorCode
pattern UserStackAssociationErrorCodeInternalError = UserStackAssociationErrorCode' "INTERNAL_ERROR"

{-# COMPLETE 
  UserStackAssociationErrorCodeStackNotFound,

  UserStackAssociationErrorCodeUserNameNotFound,

  UserStackAssociationErrorCodeDirectoryNotFound,

  UserStackAssociationErrorCodeInternalError,
  UserStackAssociationErrorCode'
  #-}
