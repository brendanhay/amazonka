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
-- Copyright   : (c) 2013-2021 Brendan Hay
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
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Internal

newtype UsageLimitFeatureType = UsageLimitFeatureType'
  { fromUsageLimitFeatureType ::
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
