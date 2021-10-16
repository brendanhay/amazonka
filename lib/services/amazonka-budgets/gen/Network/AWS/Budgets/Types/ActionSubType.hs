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
-- Module      : Network.AWS.Budgets.Types.ActionSubType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Budgets.Types.ActionSubType
  ( ActionSubType
      ( ..,
        ActionSubType_STOP_EC2_INSTANCES,
        ActionSubType_STOP_RDS_INSTANCES
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype ActionSubType = ActionSubType'
  { fromActionSubType ::
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

pattern ActionSubType_STOP_EC2_INSTANCES :: ActionSubType
pattern ActionSubType_STOP_EC2_INSTANCES = ActionSubType' "STOP_EC2_INSTANCES"

pattern ActionSubType_STOP_RDS_INSTANCES :: ActionSubType
pattern ActionSubType_STOP_RDS_INSTANCES = ActionSubType' "STOP_RDS_INSTANCES"

{-# COMPLETE
  ActionSubType_STOP_EC2_INSTANCES,
  ActionSubType_STOP_RDS_INSTANCES,
  ActionSubType'
  #-}
