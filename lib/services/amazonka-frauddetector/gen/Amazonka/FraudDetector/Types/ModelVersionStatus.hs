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
-- Module      : Amazonka.FraudDetector.Types.ModelVersionStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FraudDetector.Types.ModelVersionStatus
  ( ModelVersionStatus
      ( ..,
        ModelVersionStatus_ACTIVE,
        ModelVersionStatus_INACTIVE,
        ModelVersionStatus_TRAINING_CANCELLED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype ModelVersionStatus = ModelVersionStatus'
  { fromModelVersionStatus ::
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

pattern ModelVersionStatus_ACTIVE :: ModelVersionStatus
pattern ModelVersionStatus_ACTIVE = ModelVersionStatus' "ACTIVE"

pattern ModelVersionStatus_INACTIVE :: ModelVersionStatus
pattern ModelVersionStatus_INACTIVE = ModelVersionStatus' "INACTIVE"

pattern ModelVersionStatus_TRAINING_CANCELLED :: ModelVersionStatus
pattern ModelVersionStatus_TRAINING_CANCELLED = ModelVersionStatus' "TRAINING_CANCELLED"

{-# COMPLETE
  ModelVersionStatus_ACTIVE,
  ModelVersionStatus_INACTIVE,
  ModelVersionStatus_TRAINING_CANCELLED,
  ModelVersionStatus'
  #-}
