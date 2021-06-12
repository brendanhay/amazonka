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
-- Module      : Network.AWS.Config.Types.ConfigurationItemStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.ConfigurationItemStatus
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

import qualified Network.AWS.Core as Core

newtype ConfigurationItemStatus = ConfigurationItemStatus'
  { fromConfigurationItemStatus ::
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
