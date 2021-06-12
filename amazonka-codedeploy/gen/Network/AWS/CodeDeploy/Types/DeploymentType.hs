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
-- Module      : Network.AWS.CodeDeploy.Types.DeploymentType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.DeploymentType
  ( DeploymentType
      ( ..,
        DeploymentType_BLUE_GREEN,
        DeploymentType_IN_PLACE
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype DeploymentType = DeploymentType'
  { fromDeploymentType ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
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

pattern DeploymentType_BLUE_GREEN :: DeploymentType
pattern DeploymentType_BLUE_GREEN = DeploymentType' "BLUE_GREEN"

pattern DeploymentType_IN_PLACE :: DeploymentType
pattern DeploymentType_IN_PLACE = DeploymentType' "IN_PLACE"

{-# COMPLETE
  DeploymentType_BLUE_GREEN,
  DeploymentType_IN_PLACE,
  DeploymentType'
  #-}
