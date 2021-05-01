{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
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

import qualified Network.AWS.Prelude as Prelude

newtype ConfigurationItemStatus = ConfigurationItemStatus'
  { fromConfigurationItemStatus ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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
