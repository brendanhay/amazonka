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
-- Module      : Amazonka.EMR.Types.InstanceFleetType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMR.Types.InstanceFleetType
  ( InstanceFleetType
      ( ..,
        InstanceFleetType_CORE,
        InstanceFleetType_MASTER,
        InstanceFleetType_TASK
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype InstanceFleetType = InstanceFleetType'
  { fromInstanceFleetType ::
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

pattern InstanceFleetType_CORE :: InstanceFleetType
pattern InstanceFleetType_CORE = InstanceFleetType' "CORE"

pattern InstanceFleetType_MASTER :: InstanceFleetType
pattern InstanceFleetType_MASTER = InstanceFleetType' "MASTER"

pattern InstanceFleetType_TASK :: InstanceFleetType
pattern InstanceFleetType_TASK = InstanceFleetType' "TASK"

{-# COMPLETE
  InstanceFleetType_CORE,
  InstanceFleetType_MASTER,
  InstanceFleetType_TASK,
  InstanceFleetType'
  #-}
