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

import qualified Network.AWS.Prelude as Prelude

newtype ContainerInstanceStatus = ContainerInstanceStatus'
  { fromContainerInstanceStatus ::
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
