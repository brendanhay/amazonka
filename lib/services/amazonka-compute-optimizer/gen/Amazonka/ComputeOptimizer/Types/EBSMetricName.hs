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
-- Module      : Amazonka.ComputeOptimizer.Types.EBSMetricName
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ComputeOptimizer.Types.EBSMetricName
  ( EBSMetricName
      ( ..,
        EBSMetricName_VolumeReadBytesPerSecond,
        EBSMetricName_VolumeReadOpsPerSecond,
        EBSMetricName_VolumeWriteBytesPerSecond,
        EBSMetricName_VolumeWriteOpsPerSecond
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype EBSMetricName = EBSMetricName'
  { fromEBSMetricName ::
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
