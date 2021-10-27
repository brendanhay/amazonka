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
-- Module      : Network.AWS.GreengrassV2.Types.DeploymentHistoryFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GreengrassV2.Types.DeploymentHistoryFilter
  ( DeploymentHistoryFilter
      ( ..,
        DeploymentHistoryFilter_ALL,
        DeploymentHistoryFilter_LATEST_ONLY
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype DeploymentHistoryFilter = DeploymentHistoryFilter'
  { fromDeploymentHistoryFilter ::
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

pattern DeploymentHistoryFilter_ALL :: DeploymentHistoryFilter
pattern DeploymentHistoryFilter_ALL = DeploymentHistoryFilter' "ALL"

pattern DeploymentHistoryFilter_LATEST_ONLY :: DeploymentHistoryFilter
pattern DeploymentHistoryFilter_LATEST_ONLY = DeploymentHistoryFilter' "LATEST_ONLY"

{-# COMPLETE
  DeploymentHistoryFilter_ALL,
  DeploymentHistoryFilter_LATEST_ONLY,
  DeploymentHistoryFilter'
  #-}
