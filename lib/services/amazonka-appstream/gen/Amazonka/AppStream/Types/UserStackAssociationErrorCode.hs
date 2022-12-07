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
-- Module      : Amazonka.AppStream.Types.UserStackAssociationErrorCode
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppStream.Types.UserStackAssociationErrorCode
  ( UserStackAssociationErrorCode
      ( ..,
        UserStackAssociationErrorCode_DIRECTORY_NOT_FOUND,
        UserStackAssociationErrorCode_INTERNAL_ERROR,
        UserStackAssociationErrorCode_STACK_NOT_FOUND,
        UserStackAssociationErrorCode_USER_NAME_NOT_FOUND
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype UserStackAssociationErrorCode = UserStackAssociationErrorCode'
  { fromUserStackAssociationErrorCode ::
      Data.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
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
