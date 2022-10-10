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
-- Copyright   : (c) 2013-2022 Brendan Hay
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
import qualified Amazonka.Prelude as Prelude

newtype SchemaStatus = SchemaStatus'
  { fromSchemaStatus ::
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
