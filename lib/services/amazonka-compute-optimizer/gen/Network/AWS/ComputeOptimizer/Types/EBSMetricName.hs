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
-- Module      : Network.AWS.ComputeOptimizer.Types.EBSMetricName
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ComputeOptimizer.Types.EBSMetricName
  ( EBSMetricName
      ( ..,
        EBSMetricName_VolumeReadBytesPerSecond,
        EBSMetricName_VolumeReadOpsPerSecond,
        EBSMetricName_VolumeWriteBytesPerSecond,
        EBSMetricName_VolumeWriteOpsPerSecond
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype EBSMetricName = EBSMetricName'
  { fromEBSMetricName ::
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

pattern EBSMetricName_VolumeReadBytesPerSecond :: EBSMetricName
pattern EBSMetricName_VolumeReadBytesPerSecond = EBSMetricName' "VolumeReadBytesPerSecond"

pattern EBSMetricName_VolumeReadOpsPerSecond :: EBSMetricName
pattern EBSMetricName_VolumeReadOpsPerSecond = EBSMetricName' "VolumeReadOpsPerSecond"

pattern EBSMetricName_VolumeWriteBytesPerSecond :: EBSMetricName
pattern EBSMetricName_VolumeWriteBytesPerSecond = EBSMetricName' "VolumeWriteBytesPerSecond"

pattern EBSMetricName_VolumeWriteOpsPerSecond :: EBSMetricName
pattern EBSMetricName_VolumeWriteOpsPerSecond = EBSMetricName' "VolumeWriteOpsPerSecond"

{-# COMPLETE
  EBSMetricName_VolumeReadBytesPerSecond,
  EBSMetricName_VolumeReadOpsPerSecond,
  EBSMetricName_VolumeWriteBytesPerSecond,
  EBSMetricName_VolumeWriteOpsPerSecond,
  EBSMetricName'
  #-}
