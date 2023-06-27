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
-- Module      : Amazonka.Rekognition.Types.UserStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rekognition.Types.UserStatus
  ( UserStatus
      ( ..,
        UserStatus_ACTIVE,
        UserStatus_CREATED,
        UserStatus_CREATING,
        UserStatus_UPDATING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype UserStatus = UserStatus'
  { fromUserStatus ::
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

pattern UserStatus_ACTIVE :: UserStatus
pattern UserStatus_ACTIVE = UserStatus' "ACTIVE"

pattern UserStatus_CREATED :: UserStatus
pattern UserStatus_CREATED = UserStatus' "CREATED"

pattern UserStatus_CREATING :: UserStatus
pattern UserStatus_CREATING = UserStatus' "CREATING"

pattern UserStatus_UPDATING :: UserStatus
pattern UserStatus_UPDATING = UserStatus' "UPDATING"

{-# COMPLETE
  UserStatus_ACTIVE,
  UserStatus_CREATED,
  UserStatus_CREATING,
  UserStatus_UPDATING,
  UserStatus'
  #-}
