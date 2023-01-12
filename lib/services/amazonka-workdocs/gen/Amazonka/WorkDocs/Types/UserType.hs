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
-- Module      : Amazonka.WorkDocs.Types.UserType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkDocs.Types.UserType
  ( UserType
      ( ..,
        UserType_ADMIN,
        UserType_MINIMALUSER,
        UserType_POWERUSER,
        UserType_USER,
        UserType_WORKSPACESUSER
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype UserType = UserType'
  { fromUserType ::
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

pattern UserType_ADMIN :: UserType
pattern UserType_ADMIN = UserType' "ADMIN"

pattern UserType_MINIMALUSER :: UserType
pattern UserType_MINIMALUSER = UserType' "MINIMALUSER"

pattern UserType_POWERUSER :: UserType
pattern UserType_POWERUSER = UserType' "POWERUSER"

pattern UserType_USER :: UserType
pattern UserType_USER = UserType' "USER"

pattern UserType_WORKSPACESUSER :: UserType
pattern UserType_WORKSPACESUSER = UserType' "WORKSPACESUSER"

{-# COMPLETE
  UserType_ADMIN,
  UserType_MINIMALUSER,
  UserType_POWERUSER,
  UserType_USER,
  UserType_WORKSPACESUSER,
  UserType'
  #-}
