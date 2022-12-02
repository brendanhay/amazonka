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
-- Module      : Amazonka.Nimble.Types.LaunchProfilePlatform
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Nimble.Types.LaunchProfilePlatform
  ( LaunchProfilePlatform
      ( ..,
        LaunchProfilePlatform_LINUX,
        LaunchProfilePlatform_WINDOWS
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype LaunchProfilePlatform = LaunchProfilePlatform'
  { fromLaunchProfilePlatform ::
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

pattern LaunchProfilePlatform_LINUX :: LaunchProfilePlatform
pattern LaunchProfilePlatform_LINUX = LaunchProfilePlatform' "LINUX"

pattern LaunchProfilePlatform_WINDOWS :: LaunchProfilePlatform
pattern LaunchProfilePlatform_WINDOWS = LaunchProfilePlatform' "WINDOWS"

{-# COMPLETE
  LaunchProfilePlatform_LINUX,
  LaunchProfilePlatform_WINDOWS,
  LaunchProfilePlatform'
  #-}
