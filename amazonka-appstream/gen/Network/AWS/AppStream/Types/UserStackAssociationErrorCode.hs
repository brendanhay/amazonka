{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.Types.UserStackAssociationErrorCode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppStream.Types.UserStackAssociationErrorCode
  ( UserStackAssociationErrorCode
      ( ..,
        UserStackAssociationErrorCode_DIRECTORY_NOT_FOUND,
        UserStackAssociationErrorCode_INTERNAL_ERROR,
        UserStackAssociationErrorCode_STACK_NOT_FOUND,
        UserStackAssociationErrorCode_USER_NAME_NOT_FOUND
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype UserStackAssociationErrorCode = UserStackAssociationErrorCode'
  { fromUserStackAssociationErrorCode ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
    )

pattern UserStackAssociationErrorCode_DIRECTORY_NOT_FOUND :: UserStackAssociationErrorCode
pattern UserStackAssociationErrorCode_DIRECTORY_NOT_FOUND = UserStackAssociationErrorCode' "DIRECTORY_NOT_FOUND"

pattern UserStackAssociationErrorCode_INTERNAL_ERROR :: UserStackAssociationErrorCode
pattern UserStackAssociationErrorCode_INTERNAL_ERROR = UserStackAssociationErrorCode' "INTERNAL_ERROR"

pattern UserStackAssociationErrorCode_STACK_NOT_FOUND :: UserStackAssociationErrorCode
pattern UserStackAssociationErrorCode_STACK_NOT_FOUND = UserStackAssociationErrorCode' "STACK_NOT_FOUND"

pattern UserStackAssociationErrorCode_USER_NAME_NOT_FOUND :: UserStackAssociationErrorCode
pattern UserStackAssociationErrorCode_USER_NAME_NOT_FOUND = UserStackAssociationErrorCode' "USER_NAME_NOT_FOUND"

{-# COMPLETE
  UserStackAssociationErrorCode_DIRECTORY_NOT_FOUND,
  UserStackAssociationErrorCode_INTERNAL_ERROR,
  UserStackAssociationErrorCode_STACK_NOT_FOUND,
  UserStackAssociationErrorCode_USER_NAME_NOT_FOUND,
  UserStackAssociationErrorCode'
  #-}
