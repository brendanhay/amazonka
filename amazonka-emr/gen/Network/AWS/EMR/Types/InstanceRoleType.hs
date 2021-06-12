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
-- Module      : Network.AWS.EMR.Types.InstanceRoleType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.InstanceRoleType
  ( InstanceRoleType
      ( ..,
        InstanceRoleType_CORE,
        InstanceRoleType_MASTER,
        InstanceRoleType_TASK
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype InstanceRoleType = InstanceRoleType'
  { fromInstanceRoleType ::
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

pattern InstanceRoleType_CORE :: InstanceRoleType
pattern InstanceRoleType_CORE = InstanceRoleType' "CORE"

pattern InstanceRoleType_MASTER :: InstanceRoleType
pattern InstanceRoleType_MASTER = InstanceRoleType' "MASTER"

pattern InstanceRoleType_TASK :: InstanceRoleType
pattern InstanceRoleType_TASK = InstanceRoleType' "TASK"

{-# COMPLETE
  InstanceRoleType_CORE,
  InstanceRoleType_MASTER,
  InstanceRoleType_TASK,
  InstanceRoleType'
  #-}
