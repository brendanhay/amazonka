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
-- Module      : Amazonka.AppSync.Types.SchemaStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppSync.Types.SchemaStatus
  ( SchemaStatus
      ( ..,
        SchemaStatus_ACTIVE,
        SchemaStatus_DELETING,
        SchemaStatus_FAILED,
        SchemaStatus_NOT_APPLICABLE,
        SchemaStatus_PROCESSING,
        SchemaStatus_SUCCESS
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype SchemaStatus = SchemaStatus'
  { fromSchemaStatus ::
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

pattern SchemaStatus_ACTIVE :: SchemaStatus
pattern SchemaStatus_ACTIVE = SchemaStatus' "ACTIVE"

pattern SchemaStatus_DELETING :: SchemaStatus
pattern SchemaStatus_DELETING = SchemaStatus' "DELETING"

pattern SchemaStatus_FAILED :: SchemaStatus
pattern SchemaStatus_FAILED = SchemaStatus' "FAILED"

pattern SchemaStatus_NOT_APPLICABLE :: SchemaStatus
pattern SchemaStatus_NOT_APPLICABLE = SchemaStatus' "NOT_APPLICABLE"

pattern SchemaStatus_PROCESSING :: SchemaStatus
pattern SchemaStatus_PROCESSING = SchemaStatus' "PROCESSING"

pattern SchemaStatus_SUCCESS :: SchemaStatus
pattern SchemaStatus_SUCCESS = SchemaStatus' "SUCCESS"

{-# COMPLETE
  SchemaStatus_ACTIVE,
  SchemaStatus_DELETING,
  SchemaStatus_FAILED,
  SchemaStatus_NOT_APPLICABLE,
  SchemaStatus_PROCESSING,
  SchemaStatus_SUCCESS,
  SchemaStatus'
  #-}
