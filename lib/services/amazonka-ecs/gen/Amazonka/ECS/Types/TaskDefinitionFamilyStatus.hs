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
-- Module      : Amazonka.ECS.Types.TaskDefinitionFamilyStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECS.Types.TaskDefinitionFamilyStatus
  ( TaskDefinitionFamilyStatus
      ( ..,
        TaskDefinitionFamilyStatus_ACTIVE,
        TaskDefinitionFamilyStatus_ALL,
        TaskDefinitionFamilyStatus_INACTIVE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype TaskDefinitionFamilyStatus = TaskDefinitionFamilyStatus'
  { fromTaskDefinitionFamilyStatus ::
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

pattern TaskDefinitionFamilyStatus_ACTIVE :: TaskDefinitionFamilyStatus
pattern TaskDefinitionFamilyStatus_ACTIVE = TaskDefinitionFamilyStatus' "ACTIVE"

pattern TaskDefinitionFamilyStatus_ALL :: TaskDefinitionFamilyStatus
pattern TaskDefinitionFamilyStatus_ALL = TaskDefinitionFamilyStatus' "ALL"

pattern TaskDefinitionFamilyStatus_INACTIVE :: TaskDefinitionFamilyStatus
pattern TaskDefinitionFamilyStatus_INACTIVE = TaskDefinitionFamilyStatus' "INACTIVE"

{-# COMPLETE
  TaskDefinitionFamilyStatus_ACTIVE,
  TaskDefinitionFamilyStatus_ALL,
  TaskDefinitionFamilyStatus_INACTIVE,
  TaskDefinitionFamilyStatus'
  #-}
