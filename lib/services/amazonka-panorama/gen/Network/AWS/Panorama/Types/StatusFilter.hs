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
-- Module      : Network.AWS.Panorama.Types.StatusFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Panorama.Types.StatusFilter
  ( StatusFilter
      ( ..,
        StatusFilter_DEPLOYMENT_ERROR,
        StatusFilter_DEPLOYMENT_SUCCEEDED,
        StatusFilter_PROCESSING_DEPLOYMENT,
        StatusFilter_PROCESSING_REMOVAL,
        StatusFilter_REMOVAL_FAILED,
        StatusFilter_REMOVAL_SUCCEEDED
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype StatusFilter = StatusFilter'
  { fromStatusFilter ::
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

pattern StatusFilter_DEPLOYMENT_ERROR :: StatusFilter
pattern StatusFilter_DEPLOYMENT_ERROR = StatusFilter' "DEPLOYMENT_ERROR"

pattern StatusFilter_DEPLOYMENT_SUCCEEDED :: StatusFilter
pattern StatusFilter_DEPLOYMENT_SUCCEEDED = StatusFilter' "DEPLOYMENT_SUCCEEDED"

pattern StatusFilter_PROCESSING_DEPLOYMENT :: StatusFilter
pattern StatusFilter_PROCESSING_DEPLOYMENT = StatusFilter' "PROCESSING_DEPLOYMENT"

pattern StatusFilter_PROCESSING_REMOVAL :: StatusFilter
pattern StatusFilter_PROCESSING_REMOVAL = StatusFilter' "PROCESSING_REMOVAL"

pattern StatusFilter_REMOVAL_FAILED :: StatusFilter
pattern StatusFilter_REMOVAL_FAILED = StatusFilter' "REMOVAL_FAILED"

pattern StatusFilter_REMOVAL_SUCCEEDED :: StatusFilter
pattern StatusFilter_REMOVAL_SUCCEEDED = StatusFilter' "REMOVAL_SUCCEEDED"

{-# COMPLETE
  StatusFilter_DEPLOYMENT_ERROR,
  StatusFilter_DEPLOYMENT_SUCCEEDED,
  StatusFilter_PROCESSING_DEPLOYMENT,
  StatusFilter_PROCESSING_REMOVAL,
  StatusFilter_REMOVAL_FAILED,
  StatusFilter_REMOVAL_SUCCEEDED,
  StatusFilter'
  #-}
