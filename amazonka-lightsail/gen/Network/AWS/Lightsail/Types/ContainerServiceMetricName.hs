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
-- Module      : Network.AWS.Lightsail.Types.ContainerServiceMetricName
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.ContainerServiceMetricName
  ( ContainerServiceMetricName
      ( ..,
        ContainerServiceMetricName_CPUUtilization,
        ContainerServiceMetricName_MemoryUtilization
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype ContainerServiceMetricName = ContainerServiceMetricName'
  { fromContainerServiceMetricName ::
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

pattern ContainerServiceMetricName_CPUUtilization :: ContainerServiceMetricName
pattern ContainerServiceMetricName_CPUUtilization = ContainerServiceMetricName' "CPUUtilization"

pattern ContainerServiceMetricName_MemoryUtilization :: ContainerServiceMetricName
pattern ContainerServiceMetricName_MemoryUtilization = ContainerServiceMetricName' "MemoryUtilization"

{-# COMPLETE
  ContainerServiceMetricName_CPUUtilization,
  ContainerServiceMetricName_MemoryUtilization,
  ContainerServiceMetricName'
  #-}
