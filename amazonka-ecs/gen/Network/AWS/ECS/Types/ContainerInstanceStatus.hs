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
-- Module      : Network.AWS.ECS.Types.ContainerInstanceStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.ContainerInstanceStatus
  ( ContainerInstanceStatus
      ( ..,
        ContainerInstanceStatus_ACTIVE,
        ContainerInstanceStatus_DEREGISTERING,
        ContainerInstanceStatus_DRAINING,
        ContainerInstanceStatus_REGISTERING,
        ContainerInstanceStatus_REGISTRATION_FAILED
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype ContainerInstanceStatus = ContainerInstanceStatus'
  { fromContainerInstanceStatus ::
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

pattern ContainerInstanceStatus_ACTIVE :: ContainerInstanceStatus
pattern ContainerInstanceStatus_ACTIVE = ContainerInstanceStatus' "ACTIVE"

pattern ContainerInstanceStatus_DEREGISTERING :: ContainerInstanceStatus
pattern ContainerInstanceStatus_DEREGISTERING = ContainerInstanceStatus' "DEREGISTERING"

pattern ContainerInstanceStatus_DRAINING :: ContainerInstanceStatus
pattern ContainerInstanceStatus_DRAINING = ContainerInstanceStatus' "DRAINING"

pattern ContainerInstanceStatus_REGISTERING :: ContainerInstanceStatus
pattern ContainerInstanceStatus_REGISTERING = ContainerInstanceStatus' "REGISTERING"

pattern ContainerInstanceStatus_REGISTRATION_FAILED :: ContainerInstanceStatus
pattern ContainerInstanceStatus_REGISTRATION_FAILED = ContainerInstanceStatus' "REGISTRATION_FAILED"

{-# COMPLETE
  ContainerInstanceStatus_ACTIVE,
  ContainerInstanceStatus_DEREGISTERING,
  ContainerInstanceStatus_DRAINING,
  ContainerInstanceStatus_REGISTERING,
  ContainerInstanceStatus_REGISTRATION_FAILED,
  ContainerInstanceStatus'
  #-}
