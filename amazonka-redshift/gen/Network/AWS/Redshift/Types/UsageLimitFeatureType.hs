{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
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

import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Redshift.Internal

newtype UsageLimitFeatureType = UsageLimitFeatureType'
  { fromUsageLimitFeatureType ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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
