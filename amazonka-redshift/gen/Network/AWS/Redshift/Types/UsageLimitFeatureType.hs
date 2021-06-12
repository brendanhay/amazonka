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
-- Module      : Network.AWS.Redshift.Types.UsageLimitFeatureType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.UsageLimitFeatureType
  ( UsageLimitFeatureType
      ( ..,
        UsageLimitFeatureType_Concurrency_scaling,
        UsageLimitFeatureType_Spectrum
      ),
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Redshift.Internal

newtype UsageLimitFeatureType = UsageLimitFeatureType'
  { fromUsageLimitFeatureType ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
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

pattern UsageLimitFeatureType_Spectrum :: UsageLimitFeatureType
pattern UsageLimitFeatureType_Spectrum = UsageLimitFeatureType' "spectrum"

{-# COMPLETE
  UsageLimitFeatureType_Concurrency_scaling,
  UsageLimitFeatureType_Spectrum,
  UsageLimitFeatureType'
  #-}
