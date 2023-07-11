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
-- Module      : Amazonka.Discovery.Types.ConfigurationItemType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Discovery.Types.ConfigurationItemType
  ( ConfigurationItemType
      ( ..,
        ConfigurationItemType_APPLICATION,
        ConfigurationItemType_CONNECTION,
        ConfigurationItemType_PROCESS,
        ConfigurationItemType_SERVER
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ConfigurationItemType = ConfigurationItemType'
  { fromConfigurationItemType ::
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

pattern ConfigurationItemType_APPLICATION :: ConfigurationItemType
pattern ConfigurationItemType_APPLICATION = ConfigurationItemType' "APPLICATION"

pattern ConfigurationItemType_CONNECTION :: ConfigurationItemType
pattern ConfigurationItemType_CONNECTION = ConfigurationItemType' "CONNECTION"

pattern ConfigurationItemType_PROCESS :: ConfigurationItemType
pattern ConfigurationItemType_PROCESS = ConfigurationItemType' "PROCESS"

pattern ConfigurationItemType_SERVER :: ConfigurationItemType
pattern ConfigurationItemType_SERVER = ConfigurationItemType' "SERVER"

{-# COMPLETE
  ConfigurationItemType_APPLICATION,
  ConfigurationItemType_CONNECTION,
  ConfigurationItemType_PROCESS,
  ConfigurationItemType_SERVER,
  ConfigurationItemType'
  #-}
