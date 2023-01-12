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
-- Module      : Amazonka.EMR.Types.InstanceRoleType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMR.Types.InstanceRoleType
  ( InstanceRoleType
      ( ..,
        InstanceRoleType_CORE,
        InstanceRoleType_MASTER,
        InstanceRoleType_TASK
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype InstanceRoleType = InstanceRoleType'
  { fromInstanceRoleType ::
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
