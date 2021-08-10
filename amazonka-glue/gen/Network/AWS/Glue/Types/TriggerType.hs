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
-- Module      : Network.AWS.Glue.Types.TriggerType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.TriggerType
  ( TriggerType
      ( ..,
        TriggerType_CONDITIONAL,
        TriggerType_ON_DEMAND,
        TriggerType_SCHEDULED
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype TriggerType = TriggerType'
  { fromTriggerType ::
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

pattern TriggerType_CONDITIONAL :: TriggerType
pattern TriggerType_CONDITIONAL = TriggerType' "CONDITIONAL"

pattern TriggerType_ON_DEMAND :: TriggerType
pattern TriggerType_ON_DEMAND = TriggerType' "ON_DEMAND"

pattern TriggerType_SCHEDULED :: TriggerType
pattern TriggerType_SCHEDULED = TriggerType' "SCHEDULED"

{-# COMPLETE
  TriggerType_CONDITIONAL,
  TriggerType_ON_DEMAND,
  TriggerType_SCHEDULED,
  TriggerType'
  #-}
