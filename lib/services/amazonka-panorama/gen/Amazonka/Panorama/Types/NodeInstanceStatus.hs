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
-- Module      : Amazonka.Panorama.Types.NodeInstanceStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Panorama.Types.NodeInstanceStatus
  ( NodeInstanceStatus
      ( ..,
        NodeInstanceStatus_ERROR,
        NodeInstanceStatus_NOT_AVAILABLE,
        NodeInstanceStatus_PAUSED,
        NodeInstanceStatus_RUNNING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype NodeInstanceStatus = NodeInstanceStatus'
  { fromNodeInstanceStatus ::
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

pattern NodeInstanceStatus_ERROR :: NodeInstanceStatus
pattern NodeInstanceStatus_ERROR = NodeInstanceStatus' "ERROR"

pattern NodeInstanceStatus_NOT_AVAILABLE :: NodeInstanceStatus
pattern NodeInstanceStatus_NOT_AVAILABLE = NodeInstanceStatus' "NOT_AVAILABLE"

pattern NodeInstanceStatus_PAUSED :: NodeInstanceStatus
pattern NodeInstanceStatus_PAUSED = NodeInstanceStatus' "PAUSED"

pattern NodeInstanceStatus_RUNNING :: NodeInstanceStatus
pattern NodeInstanceStatus_RUNNING = NodeInstanceStatus' "RUNNING"

{-# COMPLETE
  NodeInstanceStatus_ERROR,
  NodeInstanceStatus_NOT_AVAILABLE,
  NodeInstanceStatus_PAUSED,
  NodeInstanceStatus_RUNNING,
  NodeInstanceStatus'
  #-}
