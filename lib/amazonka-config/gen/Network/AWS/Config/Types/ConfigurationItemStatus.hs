{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.ConfigurationItemStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.ConfigurationItemStatus
  ( ConfigurationItemStatus
      ( ConfigurationItemStatus',
        ConfigurationItemStatusOK,
        ConfigurationItemStatusResourceDiscovered,
        ConfigurationItemStatusResourceNotRecorded,
        ConfigurationItemStatusResourceDeleted,
        ConfigurationItemStatusResourceDeletedNotRecorded,
        fromConfigurationItemStatus
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype ConfigurationItemStatus = ConfigurationItemStatus'
  { fromConfigurationItemStatus ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern ConfigurationItemStatusOK :: ConfigurationItemStatus
pattern ConfigurationItemStatusOK = ConfigurationItemStatus' "OK"

pattern ConfigurationItemStatusResourceDiscovered :: ConfigurationItemStatus
pattern ConfigurationItemStatusResourceDiscovered = ConfigurationItemStatus' "ResourceDiscovered"

pattern ConfigurationItemStatusResourceNotRecorded :: ConfigurationItemStatus
pattern ConfigurationItemStatusResourceNotRecorded = ConfigurationItemStatus' "ResourceNotRecorded"

pattern ConfigurationItemStatusResourceDeleted :: ConfigurationItemStatus
pattern ConfigurationItemStatusResourceDeleted = ConfigurationItemStatus' "ResourceDeleted"

pattern ConfigurationItemStatusResourceDeletedNotRecorded :: ConfigurationItemStatus
pattern ConfigurationItemStatusResourceDeletedNotRecorded = ConfigurationItemStatus' "ResourceDeletedNotRecorded"

{-# COMPLETE
  ConfigurationItemStatusOK,
  ConfigurationItemStatusResourceDiscovered,
  ConfigurationItemStatusResourceNotRecorded,
  ConfigurationItemStatusResourceDeleted,
  ConfigurationItemStatusResourceDeletedNotRecorded,
  ConfigurationItemStatus'
  #-}
