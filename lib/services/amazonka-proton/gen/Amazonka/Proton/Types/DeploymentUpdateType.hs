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
-- Module      : Amazonka.Proton.Types.DeploymentUpdateType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Proton.Types.DeploymentUpdateType
  ( DeploymentUpdateType
      ( ..,
        DeploymentUpdateType_CURRENT_VERSION,
        DeploymentUpdateType_MAJOR_VERSION,
        DeploymentUpdateType_MINOR_VERSION,
        DeploymentUpdateType_NONE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype DeploymentUpdateType = DeploymentUpdateType'
  { fromDeploymentUpdateType ::
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

pattern DeploymentUpdateType_CURRENT_VERSION :: DeploymentUpdateType
pattern DeploymentUpdateType_CURRENT_VERSION = DeploymentUpdateType' "CURRENT_VERSION"

pattern DeploymentUpdateType_MAJOR_VERSION :: DeploymentUpdateType
pattern DeploymentUpdateType_MAJOR_VERSION = DeploymentUpdateType' "MAJOR_VERSION"

pattern DeploymentUpdateType_MINOR_VERSION :: DeploymentUpdateType
pattern DeploymentUpdateType_MINOR_VERSION = DeploymentUpdateType' "MINOR_VERSION"

pattern DeploymentUpdateType_NONE :: DeploymentUpdateType
pattern DeploymentUpdateType_NONE = DeploymentUpdateType' "NONE"

{-# COMPLETE
  DeploymentUpdateType_CURRENT_VERSION,
  DeploymentUpdateType_MAJOR_VERSION,
  DeploymentUpdateType_MINOR_VERSION,
  DeploymentUpdateType_NONE,
  DeploymentUpdateType'
  #-}
