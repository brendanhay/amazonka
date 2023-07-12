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
-- Module      : Amazonka.QuickSight.Types.ResourceStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.ResourceStatus
  ( ResourceStatus
      ( ..,
        ResourceStatus_CREATION_FAILED,
        ResourceStatus_CREATION_IN_PROGRESS,
        ResourceStatus_CREATION_SUCCESSFUL,
        ResourceStatus_DELETED,
        ResourceStatus_UPDATE_FAILED,
        ResourceStatus_UPDATE_IN_PROGRESS,
        ResourceStatus_UPDATE_SUCCESSFUL
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ResourceStatus = ResourceStatus'
  { fromResourceStatus ::
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

pattern ResourceStatus_CREATION_FAILED :: ResourceStatus
pattern ResourceStatus_CREATION_FAILED = ResourceStatus' "CREATION_FAILED"

pattern ResourceStatus_CREATION_IN_PROGRESS :: ResourceStatus
pattern ResourceStatus_CREATION_IN_PROGRESS = ResourceStatus' "CREATION_IN_PROGRESS"

pattern ResourceStatus_CREATION_SUCCESSFUL :: ResourceStatus
pattern ResourceStatus_CREATION_SUCCESSFUL = ResourceStatus' "CREATION_SUCCESSFUL"

pattern ResourceStatus_DELETED :: ResourceStatus
pattern ResourceStatus_DELETED = ResourceStatus' "DELETED"

pattern ResourceStatus_UPDATE_FAILED :: ResourceStatus
pattern ResourceStatus_UPDATE_FAILED = ResourceStatus' "UPDATE_FAILED"

pattern ResourceStatus_UPDATE_IN_PROGRESS :: ResourceStatus
pattern ResourceStatus_UPDATE_IN_PROGRESS = ResourceStatus' "UPDATE_IN_PROGRESS"

pattern ResourceStatus_UPDATE_SUCCESSFUL :: ResourceStatus
pattern ResourceStatus_UPDATE_SUCCESSFUL = ResourceStatus' "UPDATE_SUCCESSFUL"

{-# COMPLETE
  ResourceStatus_CREATION_FAILED,
  ResourceStatus_CREATION_IN_PROGRESS,
  ResourceStatus_CREATION_SUCCESSFUL,
  ResourceStatus_DELETED,
  ResourceStatus_UPDATE_FAILED,
  ResourceStatus_UPDATE_IN_PROGRESS,
  ResourceStatus_UPDATE_SUCCESSFUL,
  ResourceStatus'
  #-}
