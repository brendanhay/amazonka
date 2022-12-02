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
-- Module      : Amazonka.ECR.Types.LifecyclePolicyPreviewStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECR.Types.LifecyclePolicyPreviewStatus
  ( LifecyclePolicyPreviewStatus
      ( ..,
        LifecyclePolicyPreviewStatus_COMPLETE,
        LifecyclePolicyPreviewStatus_EXPIRED,
        LifecyclePolicyPreviewStatus_FAILED,
        LifecyclePolicyPreviewStatus_IN_PROGRESS
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype LifecyclePolicyPreviewStatus = LifecyclePolicyPreviewStatus'
  { fromLifecyclePolicyPreviewStatus ::
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

pattern LifecyclePolicyPreviewStatus_COMPLETE :: LifecyclePolicyPreviewStatus
pattern LifecyclePolicyPreviewStatus_COMPLETE = LifecyclePolicyPreviewStatus' "COMPLETE"

pattern LifecyclePolicyPreviewStatus_EXPIRED :: LifecyclePolicyPreviewStatus
pattern LifecyclePolicyPreviewStatus_EXPIRED = LifecyclePolicyPreviewStatus' "EXPIRED"

pattern LifecyclePolicyPreviewStatus_FAILED :: LifecyclePolicyPreviewStatus
pattern LifecyclePolicyPreviewStatus_FAILED = LifecyclePolicyPreviewStatus' "FAILED"

pattern LifecyclePolicyPreviewStatus_IN_PROGRESS :: LifecyclePolicyPreviewStatus
pattern LifecyclePolicyPreviewStatus_IN_PROGRESS = LifecyclePolicyPreviewStatus' "IN_PROGRESS"

{-# COMPLETE
  LifecyclePolicyPreviewStatus_COMPLETE,
  LifecyclePolicyPreviewStatus_EXPIRED,
  LifecyclePolicyPreviewStatus_FAILED,
  LifecyclePolicyPreviewStatus_IN_PROGRESS,
  LifecyclePolicyPreviewStatus'
  #-}
