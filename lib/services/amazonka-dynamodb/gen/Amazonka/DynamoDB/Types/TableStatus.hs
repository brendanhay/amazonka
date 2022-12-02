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
-- Module      : Amazonka.DynamoDB.Types.TableStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DynamoDB.Types.TableStatus
  ( TableStatus
      ( ..,
        TableStatus_ACTIVE,
        TableStatus_ARCHIVED,
        TableStatus_ARCHIVING,
        TableStatus_CREATING,
        TableStatus_DELETING,
        TableStatus_INACCESSIBLE_ENCRYPTION_CREDENTIALS,
        TableStatus_UPDATING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import Amazonka.DynamoDB.Types.AttributeValue
import Amazonka.DynamoDB.Types.WriteRequest
import qualified Amazonka.Prelude as Prelude

newtype TableStatus = TableStatus'
  { fromTableStatus ::
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

pattern TableStatus_ACTIVE :: TableStatus
pattern TableStatus_ACTIVE = TableStatus' "ACTIVE"

pattern TableStatus_ARCHIVED :: TableStatus
pattern TableStatus_ARCHIVED = TableStatus' "ARCHIVED"

pattern TableStatus_ARCHIVING :: TableStatus
pattern TableStatus_ARCHIVING = TableStatus' "ARCHIVING"

pattern TableStatus_CREATING :: TableStatus
pattern TableStatus_CREATING = TableStatus' "CREATING"

pattern TableStatus_DELETING :: TableStatus
pattern TableStatus_DELETING = TableStatus' "DELETING"

pattern TableStatus_INACCESSIBLE_ENCRYPTION_CREDENTIALS :: TableStatus
pattern TableStatus_INACCESSIBLE_ENCRYPTION_CREDENTIALS = TableStatus' "INACCESSIBLE_ENCRYPTION_CREDENTIALS"

pattern TableStatus_UPDATING :: TableStatus
pattern TableStatus_UPDATING = TableStatus' "UPDATING"

{-# COMPLETE
  TableStatus_ACTIVE,
  TableStatus_ARCHIVED,
  TableStatus_ARCHIVING,
  TableStatus_CREATING,
  TableStatus_DELETING,
  TableStatus_INACCESSIBLE_ENCRYPTION_CREDENTIALS,
  TableStatus_UPDATING,
  TableStatus'
  #-}
