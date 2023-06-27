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
-- Module      : Amazonka.FinSpace.Types.TgwStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FinSpace.Types.TgwStatus
  ( TgwStatus
      ( ..,
        TgwStatus_FAILED_UPDATE,
        TgwStatus_NONE,
        TgwStatus_SUCCESSFULLY_UPDATED,
        TgwStatus_UPDATE_REQUESTED,
        TgwStatus_UPDATING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype TgwStatus = TgwStatus'
  { fromTgwStatus ::
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

pattern TgwStatus_FAILED_UPDATE :: TgwStatus
pattern TgwStatus_FAILED_UPDATE = TgwStatus' "FAILED_UPDATE"

pattern TgwStatus_NONE :: TgwStatus
pattern TgwStatus_NONE = TgwStatus' "NONE"

pattern TgwStatus_SUCCESSFULLY_UPDATED :: TgwStatus
pattern TgwStatus_SUCCESSFULLY_UPDATED = TgwStatus' "SUCCESSFULLY_UPDATED"

pattern TgwStatus_UPDATE_REQUESTED :: TgwStatus
pattern TgwStatus_UPDATE_REQUESTED = TgwStatus' "UPDATE_REQUESTED"

pattern TgwStatus_UPDATING :: TgwStatus
pattern TgwStatus_UPDATING = TgwStatus' "UPDATING"

{-# COMPLETE
  TgwStatus_FAILED_UPDATE,
  TgwStatus_NONE,
  TgwStatus_SUCCESSFULLY_UPDATED,
  TgwStatus_UPDATE_REQUESTED,
  TgwStatus_UPDATING,
  TgwStatus'
  #-}
