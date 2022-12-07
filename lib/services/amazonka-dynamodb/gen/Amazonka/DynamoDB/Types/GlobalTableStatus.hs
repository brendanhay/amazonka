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
-- Module      : Amazonka.DynamoDB.Types.GlobalTableStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DynamoDB.Types.GlobalTableStatus
  ( GlobalTableStatus
      ( ..,
        GlobalTableStatus_ACTIVE,
        GlobalTableStatus_CREATING,
        GlobalTableStatus_DELETING,
        GlobalTableStatus_UPDATING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import Amazonka.DynamoDB.Types.AttributeValue
import Amazonka.DynamoDB.Types.WriteRequest
import qualified Amazonka.Prelude as Prelude

newtype GlobalTableStatus = GlobalTableStatus'
  { fromGlobalTableStatus ::
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

pattern GlobalTableStatus_ACTIVE :: GlobalTableStatus
pattern GlobalTableStatus_ACTIVE = GlobalTableStatus' "ACTIVE"

pattern GlobalTableStatus_CREATING :: GlobalTableStatus
pattern GlobalTableStatus_CREATING = GlobalTableStatus' "CREATING"

pattern GlobalTableStatus_DELETING :: GlobalTableStatus
pattern GlobalTableStatus_DELETING = GlobalTableStatus' "DELETING"

pattern GlobalTableStatus_UPDATING :: GlobalTableStatus
pattern GlobalTableStatus_UPDATING = GlobalTableStatus' "UPDATING"

{-# COMPLETE
  GlobalTableStatus_ACTIVE,
  GlobalTableStatus_CREATING,
  GlobalTableStatus_DELETING,
  GlobalTableStatus_UPDATING,
  GlobalTableStatus'
  #-}
