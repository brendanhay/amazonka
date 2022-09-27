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
-- Module      : Amazonka.RedshiftServerLess.Types.WorkgroupStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RedshiftServerLess.Types.WorkgroupStatus
  ( WorkgroupStatus
      ( ..,
        WorkgroupStatus_AVAILABLE,
        WorkgroupStatus_CREATING,
        WorkgroupStatus_DELETING,
        WorkgroupStatus_MODIFYING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype WorkgroupStatus = WorkgroupStatus'
  { fromWorkgroupStatus ::
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

pattern WorkgroupStatus_AVAILABLE :: WorkgroupStatus
pattern WorkgroupStatus_AVAILABLE = WorkgroupStatus' "AVAILABLE"

pattern WorkgroupStatus_CREATING :: WorkgroupStatus
pattern WorkgroupStatus_CREATING = WorkgroupStatus' "CREATING"

pattern WorkgroupStatus_DELETING :: WorkgroupStatus
pattern WorkgroupStatus_DELETING = WorkgroupStatus' "DELETING"

pattern WorkgroupStatus_MODIFYING :: WorkgroupStatus
pattern WorkgroupStatus_MODIFYING = WorkgroupStatus' "MODIFYING"

{-# COMPLETE
  WorkgroupStatus_AVAILABLE,
  WorkgroupStatus_CREATING,
  WorkgroupStatus_DELETING,
  WorkgroupStatus_MODIFYING,
  WorkgroupStatus'
  #-}
