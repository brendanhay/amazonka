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
-- Module      : Amazonka.Redshift.Types.UsageLimitFeatureType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Redshift.Types.UsageLimitFeatureType
  ( UsageLimitFeatureType
      ( ..,
        UsageLimitFeatureType_Concurrency_scaling,
        UsageLimitFeatureType_Cross_region_datasharing,
        UsageLimitFeatureType_Spectrum
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Internal

newtype UsageLimitFeatureType = UsageLimitFeatureType'
  { fromUsageLimitFeatureType ::
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

pattern UsageLimitFeatureType_Concurrency_scaling :: UsageLimitFeatureType
pattern UsageLimitFeatureType_Concurrency_scaling = UsageLimitFeatureType' "concurrency-scaling"

pattern UsageLimitFeatureType_Cross_region_datasharing :: UsageLimitFeatureType
pattern UsageLimitFeatureType_Cross_region_datasharing = UsageLimitFeatureType' "cross-region-datasharing"

pattern UsageLimitFeatureType_Spectrum :: UsageLimitFeatureType
pattern UsageLimitFeatureType_Spectrum = UsageLimitFeatureType' "spectrum"

{-# COMPLETE
  UsageLimitFeatureType_Concurrency_scaling,
  UsageLimitFeatureType_Cross_region_datasharing,
  UsageLimitFeatureType_Spectrum,
  UsageLimitFeatureType'
  #-}
