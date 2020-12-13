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
        OK,
        ResourceDiscovered,
        ResourceNotRecorded,
        ResourceDeleted,
        ResourceDeletedNotRecorded
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ConfigurationItemStatus = ConfigurationItemStatus' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern OK :: ConfigurationItemStatus
pattern OK = ConfigurationItemStatus' "OK"

pattern ResourceDiscovered :: ConfigurationItemStatus
pattern ResourceDiscovered = ConfigurationItemStatus' "ResourceDiscovered"

pattern ResourceNotRecorded :: ConfigurationItemStatus
pattern ResourceNotRecorded = ConfigurationItemStatus' "ResourceNotRecorded"

pattern ResourceDeleted :: ConfigurationItemStatus
pattern ResourceDeleted = ConfigurationItemStatus' "ResourceDeleted"

pattern ResourceDeletedNotRecorded :: ConfigurationItemStatus
pattern ResourceDeletedNotRecorded = ConfigurationItemStatus' "ResourceDeletedNotRecorded"

{-# COMPLETE
  OK,
  ResourceDiscovered,
  ResourceNotRecorded,
  ResourceDeleted,
  ResourceDeletedNotRecorded,
  ConfigurationItemStatus'
  #-}
