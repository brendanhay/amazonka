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
-- Module      : Amazonka.Glue.Types.SchemaVersionStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.SchemaVersionStatus
  ( SchemaVersionStatus
      ( ..,
        SchemaVersionStatus_AVAILABLE,
        SchemaVersionStatus_DELETING,
        SchemaVersionStatus_FAILURE,
        SchemaVersionStatus_PENDING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype SchemaVersionStatus = SchemaVersionStatus'
  { fromSchemaVersionStatus ::
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

pattern SchemaVersionStatus_AVAILABLE :: SchemaVersionStatus
pattern SchemaVersionStatus_AVAILABLE = SchemaVersionStatus' "AVAILABLE"

pattern SchemaVersionStatus_DELETING :: SchemaVersionStatus
pattern SchemaVersionStatus_DELETING = SchemaVersionStatus' "DELETING"

pattern SchemaVersionStatus_FAILURE :: SchemaVersionStatus
pattern SchemaVersionStatus_FAILURE = SchemaVersionStatus' "FAILURE"

pattern SchemaVersionStatus_PENDING :: SchemaVersionStatus
pattern SchemaVersionStatus_PENDING = SchemaVersionStatus' "PENDING"

{-# COMPLETE
  SchemaVersionStatus_AVAILABLE,
  SchemaVersionStatus_DELETING,
  SchemaVersionStatus_FAILURE,
  SchemaVersionStatus_PENDING,
  SchemaVersionStatus'
  #-}
