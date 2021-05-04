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
-- Module      : Network.AWS.Lightsail.Types.ContainerServiceDeploymentState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.ContainerServiceDeploymentState
  ( ContainerServiceDeploymentState
      ( ..,
        ContainerServiceDeploymentState_ACTIVATING,
        ContainerServiceDeploymentState_ACTIVE,
        ContainerServiceDeploymentState_FAILED,
        ContainerServiceDeploymentState_INACTIVE
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype ContainerServiceDeploymentState = ContainerServiceDeploymentState'
  { fromContainerServiceDeploymentState ::
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

pattern ContainerServiceDeploymentState_ACTIVATING :: ContainerServiceDeploymentState
pattern ContainerServiceDeploymentState_ACTIVATING = ContainerServiceDeploymentState' "ACTIVATING"

pattern ContainerServiceDeploymentState_ACTIVE :: ContainerServiceDeploymentState
pattern ContainerServiceDeploymentState_ACTIVE = ContainerServiceDeploymentState' "ACTIVE"

pattern ContainerServiceDeploymentState_FAILED :: ContainerServiceDeploymentState
pattern ContainerServiceDeploymentState_FAILED = ContainerServiceDeploymentState' "FAILED"

pattern ContainerServiceDeploymentState_INACTIVE :: ContainerServiceDeploymentState
pattern ContainerServiceDeploymentState_INACTIVE = ContainerServiceDeploymentState' "INACTIVE"

{-# COMPLETE
  ContainerServiceDeploymentState_ACTIVATING,
  ContainerServiceDeploymentState_ACTIVE,
  ContainerServiceDeploymentState_FAILED,
  ContainerServiceDeploymentState_INACTIVE,
  ContainerServiceDeploymentState'
  #-}
