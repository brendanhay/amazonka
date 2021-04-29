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
-- Module      : Network.AWS.CodeDeploy.Types.DeploymentWaitType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.DeploymentWaitType
  ( DeploymentWaitType
      ( ..,
        DeploymentWaitType_READY_WAIT,
        DeploymentWaitType_TERMINATION_WAIT
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype DeploymentWaitType = DeploymentWaitType'
  { fromDeploymentWaitType ::
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

pattern DeploymentWaitType_READY_WAIT :: DeploymentWaitType
pattern DeploymentWaitType_READY_WAIT = DeploymentWaitType' "READY_WAIT"

pattern DeploymentWaitType_TERMINATION_WAIT :: DeploymentWaitType
pattern DeploymentWaitType_TERMINATION_WAIT = DeploymentWaitType' "TERMINATION_WAIT"

{-# COMPLETE
  DeploymentWaitType_READY_WAIT,
  DeploymentWaitType_TERMINATION_WAIT,
  DeploymentWaitType'
  #-}
