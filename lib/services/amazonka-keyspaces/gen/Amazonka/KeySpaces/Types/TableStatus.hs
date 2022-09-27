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
-- Module      : Amazonka.KeySpaces.Types.TableStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KeySpaces.Types.TableStatus
  ( TableStatus
      ( ..,
        TableStatus_ACTIVE,
        TableStatus_CREATING,
        TableStatus_DELETED,
        TableStatus_DELETING,
        TableStatus_INACCESSIBLE_ENCRYPTION_CREDENTIALS,
        TableStatus_RESTORING,
        TableStatus_UPDATING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype TableStatus = TableStatus'
  { fromTableStatus ::
      Core.Text
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

pattern TableStatus_ACTIVE :: TableStatus
pattern TableStatus_ACTIVE = TableStatus' "ACTIVE"

pattern TableStatus_CREATING :: TableStatus
pattern TableStatus_CREATING = TableStatus' "CREATING"

pattern TableStatus_DELETED :: TableStatus
pattern TableStatus_DELETED = TableStatus' "DELETED"

pattern TableStatus_DELETING :: TableStatus
pattern TableStatus_DELETING = TableStatus' "DELETING"

pattern TableStatus_INACCESSIBLE_ENCRYPTION_CREDENTIALS :: TableStatus
pattern TableStatus_INACCESSIBLE_ENCRYPTION_CREDENTIALS = TableStatus' "INACCESSIBLE_ENCRYPTION_CREDENTIALS"

pattern TableStatus_RESTORING :: TableStatus
pattern TableStatus_RESTORING = TableStatus' "RESTORING"

pattern TableStatus_UPDATING :: TableStatus
pattern TableStatus_UPDATING = TableStatus' "UPDATING"

{-# COMPLETE
  TableStatus_ACTIVE,
  TableStatus_CREATING,
  TableStatus_DELETED,
  TableStatus_DELETING,
  TableStatus_INACCESSIBLE_ENCRYPTION_CREDENTIALS,
  TableStatus_RESTORING,
  TableStatus_UPDATING,
  TableStatus'
  #-}
