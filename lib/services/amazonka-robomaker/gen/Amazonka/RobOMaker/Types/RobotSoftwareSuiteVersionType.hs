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
-- Module      : Amazonka.RobOMaker.Types.RobotSoftwareSuiteVersionType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RobOMaker.Types.RobotSoftwareSuiteVersionType
  ( RobotSoftwareSuiteVersionType
      ( ..,
        RobotSoftwareSuiteVersionType_Dashing,
        RobotSoftwareSuiteVersionType_Foxy,
        RobotSoftwareSuiteVersionType_Kinetic,
        RobotSoftwareSuiteVersionType_Melodic
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype RobotSoftwareSuiteVersionType = RobotSoftwareSuiteVersionType'
  { fromRobotSoftwareSuiteVersionType ::
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

pattern RobotSoftwareSuiteVersionType_Dashing :: RobotSoftwareSuiteVersionType
pattern RobotSoftwareSuiteVersionType_Dashing = RobotSoftwareSuiteVersionType' "Dashing"

pattern RobotSoftwareSuiteVersionType_Foxy :: RobotSoftwareSuiteVersionType
pattern RobotSoftwareSuiteVersionType_Foxy = RobotSoftwareSuiteVersionType' "Foxy"

pattern RobotSoftwareSuiteVersionType_Kinetic :: RobotSoftwareSuiteVersionType
pattern RobotSoftwareSuiteVersionType_Kinetic = RobotSoftwareSuiteVersionType' "Kinetic"

pattern RobotSoftwareSuiteVersionType_Melodic :: RobotSoftwareSuiteVersionType
pattern RobotSoftwareSuiteVersionType_Melodic = RobotSoftwareSuiteVersionType' "Melodic"

{-# COMPLETE
  RobotSoftwareSuiteVersionType_Dashing,
  RobotSoftwareSuiteVersionType_Foxy,
  RobotSoftwareSuiteVersionType_Kinetic,
  RobotSoftwareSuiteVersionType_Melodic,
  RobotSoftwareSuiteVersionType'
  #-}
