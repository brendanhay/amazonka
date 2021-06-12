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
-- Module      : Network.AWS.DeviceFarm.Types.InstanceStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.InstanceStatus
  ( InstanceStatus
      ( ..,
        InstanceStatus_AVAILABLE,
        InstanceStatus_IN_USE,
        InstanceStatus_NOT_AVAILABLE,
        InstanceStatus_PREPARING
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype InstanceStatus = InstanceStatus'
  { fromInstanceStatus ::
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

pattern InstanceStatus_AVAILABLE :: InstanceStatus
pattern InstanceStatus_AVAILABLE = InstanceStatus' "AVAILABLE"

pattern InstanceStatus_IN_USE :: InstanceStatus
pattern InstanceStatus_IN_USE = InstanceStatus' "IN_USE"

pattern InstanceStatus_NOT_AVAILABLE :: InstanceStatus
pattern InstanceStatus_NOT_AVAILABLE = InstanceStatus' "NOT_AVAILABLE"

pattern InstanceStatus_PREPARING :: InstanceStatus
pattern InstanceStatus_PREPARING = InstanceStatus' "PREPARING"

{-# COMPLETE
  InstanceStatus_AVAILABLE,
  InstanceStatus_IN_USE,
  InstanceStatus_NOT_AVAILABLE,
  InstanceStatus_PREPARING,
  InstanceStatus'
  #-}
