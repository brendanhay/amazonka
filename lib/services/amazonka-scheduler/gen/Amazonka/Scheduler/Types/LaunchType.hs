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
-- Module      : Amazonka.Scheduler.Types.LaunchType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Scheduler.Types.LaunchType
  ( LaunchType
      ( ..,
        LaunchType_EC2,
        LaunchType_EXTERNAL,
        LaunchType_FARGATE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype LaunchType = LaunchType'
  { fromLaunchType ::
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

pattern LaunchType_EC2 :: LaunchType
pattern LaunchType_EC2 = LaunchType' "EC2"

pattern LaunchType_EXTERNAL :: LaunchType
pattern LaunchType_EXTERNAL = LaunchType' "EXTERNAL"

pattern LaunchType_FARGATE :: LaunchType
pattern LaunchType_FARGATE = LaunchType' "FARGATE"

{-# COMPLETE
  LaunchType_EC2,
  LaunchType_EXTERNAL,
  LaunchType_FARGATE,
  LaunchType'
  #-}
