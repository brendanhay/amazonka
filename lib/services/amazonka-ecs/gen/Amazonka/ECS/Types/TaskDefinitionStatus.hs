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
-- Module      : Amazonka.ECS.Types.TaskDefinitionStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECS.Types.TaskDefinitionStatus
  ( TaskDefinitionStatus
      ( ..,
        TaskDefinitionStatus_ACTIVE,
        TaskDefinitionStatus_DELETE_IN_PROGRESS,
        TaskDefinitionStatus_INACTIVE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype TaskDefinitionStatus = TaskDefinitionStatus'
  { fromTaskDefinitionStatus ::
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

pattern TaskDefinitionStatus_ACTIVE :: TaskDefinitionStatus
pattern TaskDefinitionStatus_ACTIVE = TaskDefinitionStatus' "ACTIVE"

pattern TaskDefinitionStatus_DELETE_IN_PROGRESS :: TaskDefinitionStatus
pattern TaskDefinitionStatus_DELETE_IN_PROGRESS = TaskDefinitionStatus' "DELETE_IN_PROGRESS"

pattern TaskDefinitionStatus_INACTIVE :: TaskDefinitionStatus
pattern TaskDefinitionStatus_INACTIVE = TaskDefinitionStatus' "INACTIVE"

{-# COMPLETE
  TaskDefinitionStatus_ACTIVE,
  TaskDefinitionStatus_DELETE_IN_PROGRESS,
  TaskDefinitionStatus_INACTIVE,
  TaskDefinitionStatus'
  #-}
