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
-- Copyright   : (c) 2013-2022 Brendan Hay
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
import qualified Amazonka.Prelude as Prelude

-- | The state that a PR-based deployment can be updated to.
newtype ResourceDeploymentStatus = ResourceDeploymentStatus'
  { fromResourceDeploymentStatus ::
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
