{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.Types.UserStackAssociationErrorCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppStream.Types.UserStackAssociationErrorCode
  ( UserStackAssociationErrorCode
      ( UserStackAssociationErrorCode',
        USAECStackNotFound,
        USAECUserNameNotFound,
        USAECDirectoryNotFound,
        USAECInternalError
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype UserStackAssociationErrorCode = UserStackAssociationErrorCode' Lude.Text
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

pattern USAECStackNotFound :: UserStackAssociationErrorCode
pattern USAECStackNotFound = UserStackAssociationErrorCode' "STACK_NOT_FOUND"

pattern USAECUserNameNotFound :: UserStackAssociationErrorCode
pattern USAECUserNameNotFound = UserStackAssociationErrorCode' "USER_NAME_NOT_FOUND"

pattern USAECDirectoryNotFound :: UserStackAssociationErrorCode
pattern USAECDirectoryNotFound = UserStackAssociationErrorCode' "DIRECTORY_NOT_FOUND"

pattern USAECInternalError :: UserStackAssociationErrorCode
pattern USAECInternalError = UserStackAssociationErrorCode' "INTERNAL_ERROR"

{-# COMPLETE
  USAECStackNotFound,
  USAECUserNameNotFound,
  USAECDirectoryNotFound,
  USAECInternalError,
  UserStackAssociationErrorCode'
  #-}
