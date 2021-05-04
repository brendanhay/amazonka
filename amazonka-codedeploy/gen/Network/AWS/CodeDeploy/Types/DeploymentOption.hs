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
-- Module      : Network.AWS.CodeDeploy.Types.DeploymentOption
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.DeploymentOption
  ( DeploymentOption
      ( ..,
        DeploymentOption_WITHOUT_TRAFFIC_CONTROL,
        DeploymentOption_WITH_TRAFFIC_CONTROL
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype DeploymentOption = DeploymentOption'
  { fromDeploymentOption ::
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

pattern DeploymentOption_WITHOUT_TRAFFIC_CONTROL :: DeploymentOption
pattern DeploymentOption_WITHOUT_TRAFFIC_CONTROL = DeploymentOption' "WITHOUT_TRAFFIC_CONTROL"

pattern DeploymentOption_WITH_TRAFFIC_CONTROL :: DeploymentOption
pattern DeploymentOption_WITH_TRAFFIC_CONTROL = DeploymentOption' "WITH_TRAFFIC_CONTROL"

{-# COMPLETE
  DeploymentOption_WITHOUT_TRAFFIC_CONTROL,
  DeploymentOption_WITH_TRAFFIC_CONTROL,
  DeploymentOption'
  #-}
