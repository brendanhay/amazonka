{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.DeploymentStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.DeploymentStatus
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

import qualified Network.AWS.Prelude as Prelude

newtype DeploymentStatus = DeploymentStatus'
  { fromDeploymentStatus ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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
