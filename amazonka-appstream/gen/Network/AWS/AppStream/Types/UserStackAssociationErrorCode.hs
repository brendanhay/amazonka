{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
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

import qualified Network.AWS.Core as Core

newtype UserStackAssociationErrorCode = UserStackAssociationErrorCode'
  { fromUserStackAssociationErrorCode ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
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
