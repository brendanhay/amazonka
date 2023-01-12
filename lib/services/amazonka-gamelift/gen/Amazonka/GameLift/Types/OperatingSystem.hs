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
-- Module      : Amazonka.GameLift.Types.OperatingSystem
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GameLift.Types.OperatingSystem
  ( OperatingSystem
      ( ..,
        OperatingSystem_AMAZON_LINUX,
        OperatingSystem_AMAZON_LINUX_2,
        OperatingSystem_WINDOWS_2012
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype OperatingSystem = OperatingSystem'
  { fromOperatingSystem ::
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

pattern OperatingSystem_AMAZON_LINUX :: OperatingSystem
pattern OperatingSystem_AMAZON_LINUX = OperatingSystem' "AMAZON_LINUX"

pattern OperatingSystem_AMAZON_LINUX_2 :: OperatingSystem
pattern OperatingSystem_AMAZON_LINUX_2 = OperatingSystem' "AMAZON_LINUX_2"

pattern OperatingSystem_WINDOWS_2012 :: OperatingSystem
pattern OperatingSystem_WINDOWS_2012 = OperatingSystem' "WINDOWS_2012"

{-# COMPLETE
  OperatingSystem_AMAZON_LINUX,
  OperatingSystem_AMAZON_LINUX_2,
  OperatingSystem_WINDOWS_2012,
  OperatingSystem'
  #-}
