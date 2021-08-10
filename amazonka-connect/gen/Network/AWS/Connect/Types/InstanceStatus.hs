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
-- Module      : Network.AWS.Connect.Types.InstanceStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.InstanceStatus
  ( InstanceStatus
      ( ..,
        InstanceStatus_ACTIVE,
        InstanceStatus_CREATION_FAILED,
        InstanceStatus_CREATION_IN_PROGRESS
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype InstanceStatus = InstanceStatus'
  { fromInstanceStatus ::
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

pattern InstanceStatus_ACTIVE :: InstanceStatus
pattern InstanceStatus_ACTIVE = InstanceStatus' "ACTIVE"

pattern InstanceStatus_CREATION_FAILED :: InstanceStatus
pattern InstanceStatus_CREATION_FAILED = InstanceStatus' "CREATION_FAILED"

pattern InstanceStatus_CREATION_IN_PROGRESS :: InstanceStatus
pattern InstanceStatus_CREATION_IN_PROGRESS = InstanceStatus' "CREATION_IN_PROGRESS"

{-# COMPLETE
  InstanceStatus_ACTIVE,
  InstanceStatus_CREATION_FAILED,
  InstanceStatus_CREATION_IN_PROGRESS,
  InstanceStatus'
  #-}
