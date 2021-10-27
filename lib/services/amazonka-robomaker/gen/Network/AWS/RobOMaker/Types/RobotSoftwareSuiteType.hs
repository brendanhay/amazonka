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
-- Module      : Network.AWS.RobOMaker.Types.RobotSoftwareSuiteType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RobOMaker.Types.RobotSoftwareSuiteType
  ( RobotSoftwareSuiteType
      ( ..,
        RobotSoftwareSuiteType_General,
        RobotSoftwareSuiteType_ROS,
        RobotSoftwareSuiteType_ROS2
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype RobotSoftwareSuiteType = RobotSoftwareSuiteType'
  { fromRobotSoftwareSuiteType ::
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

pattern RobotSoftwareSuiteType_General :: RobotSoftwareSuiteType
pattern RobotSoftwareSuiteType_General = RobotSoftwareSuiteType' "General"

pattern RobotSoftwareSuiteType_ROS :: RobotSoftwareSuiteType
pattern RobotSoftwareSuiteType_ROS = RobotSoftwareSuiteType' "ROS"

pattern RobotSoftwareSuiteType_ROS2 :: RobotSoftwareSuiteType
pattern RobotSoftwareSuiteType_ROS2 = RobotSoftwareSuiteType' "ROS2"

{-# COMPLETE
  RobotSoftwareSuiteType_General,
  RobotSoftwareSuiteType_ROS,
  RobotSoftwareSuiteType_ROS2,
  RobotSoftwareSuiteType'
  #-}
