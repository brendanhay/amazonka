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
-- Module      : Amazonka.ECS.Types.OSFamily
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECS.Types.OSFamily
  ( OSFamily
      ( ..,
        OSFamily_LINUX,
        OSFamily_WINDOWS_SERVER_2004_CORE,
        OSFamily_WINDOWS_SERVER_2016_FULL,
        OSFamily_WINDOWS_SERVER_2019_CORE,
        OSFamily_WINDOWS_SERVER_2019_FULL,
        OSFamily_WINDOWS_SERVER_2022_CORE,
        OSFamily_WINDOWS_SERVER_2022_FULL,
        OSFamily_WINDOWS_SERVER_20H2_CORE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype OSFamily = OSFamily'
  { fromOSFamily ::
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

pattern OSFamily_LINUX :: OSFamily
pattern OSFamily_LINUX = OSFamily' "LINUX"

pattern OSFamily_WINDOWS_SERVER_2004_CORE :: OSFamily
pattern OSFamily_WINDOWS_SERVER_2004_CORE = OSFamily' "WINDOWS_SERVER_2004_CORE"

pattern OSFamily_WINDOWS_SERVER_2016_FULL :: OSFamily
pattern OSFamily_WINDOWS_SERVER_2016_FULL = OSFamily' "WINDOWS_SERVER_2016_FULL"

pattern OSFamily_WINDOWS_SERVER_2019_CORE :: OSFamily
pattern OSFamily_WINDOWS_SERVER_2019_CORE = OSFamily' "WINDOWS_SERVER_2019_CORE"

pattern OSFamily_WINDOWS_SERVER_2019_FULL :: OSFamily
pattern OSFamily_WINDOWS_SERVER_2019_FULL = OSFamily' "WINDOWS_SERVER_2019_FULL"

pattern OSFamily_WINDOWS_SERVER_2022_CORE :: OSFamily
pattern OSFamily_WINDOWS_SERVER_2022_CORE = OSFamily' "WINDOWS_SERVER_2022_CORE"

pattern OSFamily_WINDOWS_SERVER_2022_FULL :: OSFamily
pattern OSFamily_WINDOWS_SERVER_2022_FULL = OSFamily' "WINDOWS_SERVER_2022_FULL"

pattern OSFamily_WINDOWS_SERVER_20H2_CORE :: OSFamily
pattern OSFamily_WINDOWS_SERVER_20H2_CORE = OSFamily' "WINDOWS_SERVER_20H2_CORE"

{-# COMPLETE
  OSFamily_LINUX,
  OSFamily_WINDOWS_SERVER_2004_CORE,
  OSFamily_WINDOWS_SERVER_2016_FULL,
  OSFamily_WINDOWS_SERVER_2019_CORE,
  OSFamily_WINDOWS_SERVER_2019_FULL,
  OSFamily_WINDOWS_SERVER_2022_CORE,
  OSFamily_WINDOWS_SERVER_2022_FULL,
  OSFamily_WINDOWS_SERVER_20H2_CORE,
  OSFamily'
  #-}
