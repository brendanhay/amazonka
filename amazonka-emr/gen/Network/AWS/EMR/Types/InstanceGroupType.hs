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
-- Module      : Network.AWS.EMR.Types.InstanceGroupType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.InstanceGroupType
  ( InstanceGroupType
      ( ..,
        InstanceGroupType_CORE,
        InstanceGroupType_MASTER,
        InstanceGroupType_TASK
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype InstanceGroupType = InstanceGroupType'
  { fromInstanceGroupType ::
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

pattern InstanceGroupType_CORE :: InstanceGroupType
pattern InstanceGroupType_CORE = InstanceGroupType' "CORE"

pattern InstanceGroupType_MASTER :: InstanceGroupType
pattern InstanceGroupType_MASTER = InstanceGroupType' "MASTER"

pattern InstanceGroupType_TASK :: InstanceGroupType
pattern InstanceGroupType_TASK = InstanceGroupType' "TASK"

{-# COMPLETE
  InstanceGroupType_CORE,
  InstanceGroupType_MASTER,
  InstanceGroupType_TASK,
  InstanceGroupType'
  #-}
