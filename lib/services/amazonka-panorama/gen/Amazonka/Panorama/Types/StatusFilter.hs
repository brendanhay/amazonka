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
-- Module      : Amazonka.Panorama.Types.StatusFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Panorama.Types.StatusFilter
  ( StatusFilter
      ( ..,
        StatusFilter_DEPLOYMENT_ERROR,
        StatusFilter_DEPLOYMENT_FAILED,
        StatusFilter_DEPLOYMENT_SUCCEEDED,
        StatusFilter_PROCESSING_DEPLOYMENT,
        StatusFilter_PROCESSING_REMOVAL,
        StatusFilter_REMOVAL_FAILED,
        StatusFilter_REMOVAL_SUCCEEDED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype StatusFilter = StatusFilter'
  { fromStatusFilter ::
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

pattern StatusFilter_DEPLOYMENT_ERROR :: StatusFilter
pattern StatusFilter_DEPLOYMENT_ERROR = StatusFilter' "DEPLOYMENT_ERROR"

pattern StatusFilter_DEPLOYMENT_FAILED :: StatusFilter
pattern StatusFilter_DEPLOYMENT_FAILED = StatusFilter' "DEPLOYMENT_FAILED"

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
  StatusFilter_DEPLOYMENT_FAILED,
  StatusFilter_DEPLOYMENT_SUCCEEDED,
  StatusFilter_PROCESSING_DEPLOYMENT,
  StatusFilter_PROCESSING_REMOVAL,
  StatusFilter_REMOVAL_FAILED,
  StatusFilter_REMOVAL_SUCCEEDED,
  StatusFilter'
  #-}
