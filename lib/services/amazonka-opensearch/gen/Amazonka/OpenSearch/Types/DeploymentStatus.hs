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
-- Module      : Amazonka.OpenSearch.Types.DeploymentStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpenSearch.Types.DeploymentStatus
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
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype DeploymentStatus = DeploymentStatus'
  { fromDeploymentStatus ::
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
