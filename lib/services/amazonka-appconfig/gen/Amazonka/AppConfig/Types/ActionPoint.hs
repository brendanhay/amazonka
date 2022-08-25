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
-- Module      : Amazonka.AppConfig.Types.ActionPoint
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppConfig.Types.ActionPoint
  ( ActionPoint
      ( ..,
        ActionPoint_ON_DEPLOYMENT_BAKING,
        ActionPoint_ON_DEPLOYMENT_COMPLETE,
        ActionPoint_ON_DEPLOYMENT_ROLLED_BACK,
        ActionPoint_ON_DEPLOYMENT_START,
        ActionPoint_ON_DEPLOYMENT_STEP,
        ActionPoint_PRE_CREATE_HOSTED_CONFIGURATION_VERSION,
        ActionPoint_PRE_START_DEPLOYMENT
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype ActionPoint = ActionPoint'
  { fromActionPoint ::
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

pattern ActionPoint_ON_DEPLOYMENT_BAKING :: ActionPoint
pattern ActionPoint_ON_DEPLOYMENT_BAKING = ActionPoint' "ON_DEPLOYMENT_BAKING"

pattern ActionPoint_ON_DEPLOYMENT_COMPLETE :: ActionPoint
pattern ActionPoint_ON_DEPLOYMENT_COMPLETE = ActionPoint' "ON_DEPLOYMENT_COMPLETE"

pattern ActionPoint_ON_DEPLOYMENT_ROLLED_BACK :: ActionPoint
pattern ActionPoint_ON_DEPLOYMENT_ROLLED_BACK = ActionPoint' "ON_DEPLOYMENT_ROLLED_BACK"

pattern ActionPoint_ON_DEPLOYMENT_START :: ActionPoint
pattern ActionPoint_ON_DEPLOYMENT_START = ActionPoint' "ON_DEPLOYMENT_START"

pattern ActionPoint_ON_DEPLOYMENT_STEP :: ActionPoint
pattern ActionPoint_ON_DEPLOYMENT_STEP = ActionPoint' "ON_DEPLOYMENT_STEP"

pattern ActionPoint_PRE_CREATE_HOSTED_CONFIGURATION_VERSION :: ActionPoint
pattern ActionPoint_PRE_CREATE_HOSTED_CONFIGURATION_VERSION = ActionPoint' "PRE_CREATE_HOSTED_CONFIGURATION_VERSION"

pattern ActionPoint_PRE_START_DEPLOYMENT :: ActionPoint
pattern ActionPoint_PRE_START_DEPLOYMENT = ActionPoint' "PRE_START_DEPLOYMENT"

{-# COMPLETE
  ActionPoint_ON_DEPLOYMENT_BAKING,
  ActionPoint_ON_DEPLOYMENT_COMPLETE,
  ActionPoint_ON_DEPLOYMENT_ROLLED_BACK,
  ActionPoint_ON_DEPLOYMENT_START,
  ActionPoint_ON_DEPLOYMENT_STEP,
  ActionPoint_PRE_CREATE_HOSTED_CONFIGURATION_VERSION,
  ActionPoint_PRE_START_DEPLOYMENT,
  ActionPoint'
  #-}
