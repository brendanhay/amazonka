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
-- Module      : Amazonka.Rekognition.Types.LivenessSessionStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rekognition.Types.LivenessSessionStatus
  ( LivenessSessionStatus
      ( ..,
        LivenessSessionStatus_CREATED,
        LivenessSessionStatus_EXPIRED,
        LivenessSessionStatus_FAILED,
        LivenessSessionStatus_IN_PROGRESS,
        LivenessSessionStatus_SUCCEEDED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype LivenessSessionStatus = LivenessSessionStatus'
  { fromLivenessSessionStatus ::
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

pattern LivenessSessionStatus_CREATED :: LivenessSessionStatus
pattern LivenessSessionStatus_CREATED = LivenessSessionStatus' "CREATED"

pattern LivenessSessionStatus_EXPIRED :: LivenessSessionStatus
pattern LivenessSessionStatus_EXPIRED = LivenessSessionStatus' "EXPIRED"

pattern LivenessSessionStatus_FAILED :: LivenessSessionStatus
pattern LivenessSessionStatus_FAILED = LivenessSessionStatus' "FAILED"

pattern LivenessSessionStatus_IN_PROGRESS :: LivenessSessionStatus
pattern LivenessSessionStatus_IN_PROGRESS = LivenessSessionStatus' "IN_PROGRESS"

pattern LivenessSessionStatus_SUCCEEDED :: LivenessSessionStatus
pattern LivenessSessionStatus_SUCCEEDED = LivenessSessionStatus' "SUCCEEDED"

{-# COMPLETE
  LivenessSessionStatus_CREATED,
  LivenessSessionStatus_EXPIRED,
  LivenessSessionStatus_FAILED,
  LivenessSessionStatus_IN_PROGRESS,
  LivenessSessionStatus_SUCCEEDED,
  LivenessSessionStatus'
  #-}
