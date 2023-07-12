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
-- Module      : Amazonka.EMR.Types.InstanceGroupType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMR.Types.InstanceGroupType
  ( InstanceGroupType
      ( ..,
        InstanceGroupType_CORE,
        InstanceGroupType_MASTER,
        InstanceGroupType_TASK
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype InstanceGroupType = InstanceGroupType'
  { fromInstanceGroupType ::
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
