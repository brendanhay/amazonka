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
-- Module      : Amazonka.SageMaker.Types.StageStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.StageStatus
  ( StageStatus
      ( ..,
        StageStatus_CREATING,
        StageStatus_DEPLOYED,
        StageStatus_FAILED,
        StageStatus_INPROGRESS,
        StageStatus_READYTODEPLOY,
        StageStatus_STARTING,
        StageStatus_STOPPED,
        StageStatus_STOPPING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype StageStatus = StageStatus'
  { fromStageStatus ::
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

pattern StageStatus_CREATING :: StageStatus
pattern StageStatus_CREATING = StageStatus' "CREATING"

pattern StageStatus_DEPLOYED :: StageStatus
pattern StageStatus_DEPLOYED = StageStatus' "DEPLOYED"

pattern StageStatus_FAILED :: StageStatus
pattern StageStatus_FAILED = StageStatus' "FAILED"

pattern StageStatus_INPROGRESS :: StageStatus
pattern StageStatus_INPROGRESS = StageStatus' "INPROGRESS"

pattern StageStatus_READYTODEPLOY :: StageStatus
pattern StageStatus_READYTODEPLOY = StageStatus' "READYTODEPLOY"

pattern StageStatus_STARTING :: StageStatus
pattern StageStatus_STARTING = StageStatus' "STARTING"

pattern StageStatus_STOPPED :: StageStatus
pattern StageStatus_STOPPED = StageStatus' "STOPPED"

pattern StageStatus_STOPPING :: StageStatus
pattern StageStatus_STOPPING = StageStatus' "STOPPING"

{-# COMPLETE
  StageStatus_CREATING,
  StageStatus_DEPLOYED,
  StageStatus_FAILED,
  StageStatus_INPROGRESS,
  StageStatus_READYTODEPLOY,
  StageStatus_STARTING,
  StageStatus_STOPPED,
  StageStatus_STOPPING,
  StageStatus'
  #-}
