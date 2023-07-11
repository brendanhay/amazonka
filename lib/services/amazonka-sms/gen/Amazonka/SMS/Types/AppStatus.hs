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
-- Module      : Amazonka.SMS.Types.AppStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SMS.Types.AppStatus
  ( AppStatus
      ( ..,
        AppStatus_ACTIVE,
        AppStatus_CREATING,
        AppStatus_DELETED,
        AppStatus_DELETE_FAILED,
        AppStatus_DELETING,
        AppStatus_UPDATING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype AppStatus = AppStatus'
  { fromAppStatus ::
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

pattern AppStatus_ACTIVE :: AppStatus
pattern AppStatus_ACTIVE = AppStatus' "ACTIVE"

pattern AppStatus_CREATING :: AppStatus
pattern AppStatus_CREATING = AppStatus' "CREATING"

pattern AppStatus_DELETED :: AppStatus
pattern AppStatus_DELETED = AppStatus' "DELETED"

pattern AppStatus_DELETE_FAILED :: AppStatus
pattern AppStatus_DELETE_FAILED = AppStatus' "DELETE_FAILED"

pattern AppStatus_DELETING :: AppStatus
pattern AppStatus_DELETING = AppStatus' "DELETING"

pattern AppStatus_UPDATING :: AppStatus
pattern AppStatus_UPDATING = AppStatus' "UPDATING"

{-# COMPLETE
  AppStatus_ACTIVE,
  AppStatus_CREATING,
  AppStatus_DELETED,
  AppStatus_DELETE_FAILED,
  AppStatus_DELETING,
  AppStatus_UPDATING,
  AppStatus'
  #-}
