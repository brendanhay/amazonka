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
-- Module      : Amazonka.Glue.Types.TriggerType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.TriggerType
  ( TriggerType
      ( ..,
        TriggerType_CONDITIONAL,
        TriggerType_EVENT,
        TriggerType_ON_DEMAND,
        TriggerType_SCHEDULED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype TriggerType = TriggerType'
  { fromTriggerType ::
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

pattern TriggerType_CONDITIONAL :: TriggerType
pattern TriggerType_CONDITIONAL = TriggerType' "CONDITIONAL"

pattern TriggerType_EVENT :: TriggerType
pattern TriggerType_EVENT = TriggerType' "EVENT"

pattern TriggerType_ON_DEMAND :: TriggerType
pattern TriggerType_ON_DEMAND = TriggerType' "ON_DEMAND"

pattern TriggerType_SCHEDULED :: TriggerType
pattern TriggerType_SCHEDULED = TriggerType' "SCHEDULED"

{-# COMPLETE
  TriggerType_CONDITIONAL,
  TriggerType_EVENT,
  TriggerType_ON_DEMAND,
  TriggerType_SCHEDULED,
  TriggerType'
  #-}
