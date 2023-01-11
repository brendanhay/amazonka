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
-- Module      : Amazonka.Config.Types.ConfigurationItemStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Config.Types.ConfigurationItemStatus
  ( ConfigurationItemStatus
      ( ..,
        ConfigurationItemStatus_OK,
        ConfigurationItemStatus_ResourceDeleted,
        ConfigurationItemStatus_ResourceDeletedNotRecorded,
        ConfigurationItemStatus_ResourceDiscovered,
        ConfigurationItemStatus_ResourceNotRecorded
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ConfigurationItemStatus = ConfigurationItemStatus'
  { fromConfigurationItemStatus ::
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

pattern ConfigurationItemStatus_OK :: ConfigurationItemStatus
pattern ConfigurationItemStatus_OK = ConfigurationItemStatus' "OK"

pattern ConfigurationItemStatus_ResourceDeleted :: ConfigurationItemStatus
pattern ConfigurationItemStatus_ResourceDeleted = ConfigurationItemStatus' "ResourceDeleted"

pattern ConfigurationItemStatus_ResourceDeletedNotRecorded :: ConfigurationItemStatus
pattern ConfigurationItemStatus_ResourceDeletedNotRecorded = ConfigurationItemStatus' "ResourceDeletedNotRecorded"

pattern ConfigurationItemStatus_ResourceDiscovered :: ConfigurationItemStatus
pattern ConfigurationItemStatus_ResourceDiscovered = ConfigurationItemStatus' "ResourceDiscovered"

pattern ConfigurationItemStatus_ResourceNotRecorded :: ConfigurationItemStatus
pattern ConfigurationItemStatus_ResourceNotRecorded = ConfigurationItemStatus' "ResourceNotRecorded"

{-# COMPLETE
  ConfigurationItemStatus_OK,
  ConfigurationItemStatus_ResourceDeleted,
  ConfigurationItemStatus_ResourceDeletedNotRecorded,
  ConfigurationItemStatus_ResourceDiscovered,
  ConfigurationItemStatus_ResourceNotRecorded,
  ConfigurationItemStatus'
  #-}
