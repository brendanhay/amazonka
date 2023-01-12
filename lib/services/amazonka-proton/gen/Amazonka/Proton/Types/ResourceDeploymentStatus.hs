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
-- Module      : Amazonka.Proton.Types.ResourceDeploymentStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Proton.Types.ResourceDeploymentStatus
  ( ResourceDeploymentStatus
      ( ..,
        ResourceDeploymentStatus_FAILED,
        ResourceDeploymentStatus_IN_PROGRESS,
        ResourceDeploymentStatus_SUCCEEDED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The state that a PR-based deployment can be updated to.
newtype ResourceDeploymentStatus = ResourceDeploymentStatus'
  { fromResourceDeploymentStatus ::
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

pattern ResourceDeploymentStatus_FAILED :: ResourceDeploymentStatus
pattern ResourceDeploymentStatus_FAILED = ResourceDeploymentStatus' "FAILED"

pattern ResourceDeploymentStatus_IN_PROGRESS :: ResourceDeploymentStatus
pattern ResourceDeploymentStatus_IN_PROGRESS = ResourceDeploymentStatus' "IN_PROGRESS"

pattern ResourceDeploymentStatus_SUCCEEDED :: ResourceDeploymentStatus
pattern ResourceDeploymentStatus_SUCCEEDED = ResourceDeploymentStatus' "SUCCEEDED"

{-# COMPLETE
  ResourceDeploymentStatus_FAILED,
  ResourceDeploymentStatus_IN_PROGRESS,
  ResourceDeploymentStatus_SUCCEEDED,
  ResourceDeploymentStatus'
  #-}
