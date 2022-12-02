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
-- Module      : Amazonka.ECS.Types.ContainerInstanceStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECS.Types.ContainerInstanceStatus
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ContainerInstanceStatus = ContainerInstanceStatus'
  { fromContainerInstanceStatus ::
      Data.Text
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
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
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
