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
-- Module      : Amazonka.ElasticSearch.Types.DeploymentStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElasticSearch.Types.DeploymentStatus
  ( DeploymentStatus
      ( ..,
        DeploymentStatus_COMPLETED,
        DeploymentStatus_ELIGIBLE,
        DeploymentStatus_IN_PROGRESS,
        DeploymentStatus_NOT_ELIGIBLE,
        DeploymentStatus_PENDING_UPDATE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype DeploymentStatus = DeploymentStatus'
  { fromDeploymentStatus ::
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

pattern DeploymentStatus_COMPLETED :: DeploymentStatus
pattern DeploymentStatus_COMPLETED = DeploymentStatus' "COMPLETED"

pattern DeploymentStatus_ELIGIBLE :: DeploymentStatus
pattern DeploymentStatus_ELIGIBLE = DeploymentStatus' "ELIGIBLE"

pattern DeploymentStatus_IN_PROGRESS :: DeploymentStatus
pattern DeploymentStatus_IN_PROGRESS = DeploymentStatus' "IN_PROGRESS"

pattern DeploymentStatus_NOT_ELIGIBLE :: DeploymentStatus
pattern DeploymentStatus_NOT_ELIGIBLE = DeploymentStatus' "NOT_ELIGIBLE"

pattern DeploymentStatus_PENDING_UPDATE :: DeploymentStatus
pattern DeploymentStatus_PENDING_UPDATE = DeploymentStatus' "PENDING_UPDATE"

{-# COMPLETE
  DeploymentStatus_COMPLETED,
  DeploymentStatus_ELIGIBLE,
  DeploymentStatus_IN_PROGRESS,
  DeploymentStatus_NOT_ELIGIBLE,
  DeploymentStatus_PENDING_UPDATE,
  DeploymentStatus'
  #-}
