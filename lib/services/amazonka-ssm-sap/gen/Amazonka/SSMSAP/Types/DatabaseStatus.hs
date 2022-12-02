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
-- Module      : Amazonka.SSMSAP.Types.DatabaseStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSMSAP.Types.DatabaseStatus
  ( DatabaseStatus
      ( ..,
        DatabaseStatus_RUNNING,
        DatabaseStatus_STARTING,
        DatabaseStatus_STOPPED,
        DatabaseStatus_UNKNOWN,
        DatabaseStatus_WARNING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype DatabaseStatus = DatabaseStatus'
  { fromDatabaseStatus ::
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

pattern DatabaseStatus_RUNNING :: DatabaseStatus
pattern DatabaseStatus_RUNNING = DatabaseStatus' "RUNNING"

pattern DatabaseStatus_STARTING :: DatabaseStatus
pattern DatabaseStatus_STARTING = DatabaseStatus' "STARTING"

pattern DatabaseStatus_STOPPED :: DatabaseStatus
pattern DatabaseStatus_STOPPED = DatabaseStatus' "STOPPED"

pattern DatabaseStatus_UNKNOWN :: DatabaseStatus
pattern DatabaseStatus_UNKNOWN = DatabaseStatus' "UNKNOWN"

pattern DatabaseStatus_WARNING :: DatabaseStatus
pattern DatabaseStatus_WARNING = DatabaseStatus' "WARNING"

{-# COMPLETE
  DatabaseStatus_RUNNING,
  DatabaseStatus_STARTING,
  DatabaseStatus_STOPPED,
  DatabaseStatus_UNKNOWN,
  DatabaseStatus_WARNING,
  DatabaseStatus'
  #-}
