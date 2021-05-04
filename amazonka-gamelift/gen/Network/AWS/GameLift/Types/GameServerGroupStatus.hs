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
-- Module      : Network.AWS.GameLift.Types.GameServerGroupStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.GameServerGroupStatus
  ( GameServerGroupStatus
      ( ..,
        GameServerGroupStatus_ACTIVATING,
        GameServerGroupStatus_ACTIVE,
        GameServerGroupStatus_DELETED,
        GameServerGroupStatus_DELETE_SCHEDULED,
        GameServerGroupStatus_DELETING,
        GameServerGroupStatus_ERROR,
        GameServerGroupStatus_NEW
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype GameServerGroupStatus = GameServerGroupStatus'
  { fromGameServerGroupStatus ::
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

pattern GameServerGroupStatus_ACTIVATING :: GameServerGroupStatus
pattern GameServerGroupStatus_ACTIVATING = GameServerGroupStatus' "ACTIVATING"

pattern GameServerGroupStatus_ACTIVE :: GameServerGroupStatus
pattern GameServerGroupStatus_ACTIVE = GameServerGroupStatus' "ACTIVE"

pattern GameServerGroupStatus_DELETED :: GameServerGroupStatus
pattern GameServerGroupStatus_DELETED = GameServerGroupStatus' "DELETED"

pattern GameServerGroupStatus_DELETE_SCHEDULED :: GameServerGroupStatus
pattern GameServerGroupStatus_DELETE_SCHEDULED = GameServerGroupStatus' "DELETE_SCHEDULED"

pattern GameServerGroupStatus_DELETING :: GameServerGroupStatus
pattern GameServerGroupStatus_DELETING = GameServerGroupStatus' "DELETING"

pattern GameServerGroupStatus_ERROR :: GameServerGroupStatus
pattern GameServerGroupStatus_ERROR = GameServerGroupStatus' "ERROR"

pattern GameServerGroupStatus_NEW :: GameServerGroupStatus
pattern GameServerGroupStatus_NEW = GameServerGroupStatus' "NEW"

{-# COMPLETE
  GameServerGroupStatus_ACTIVATING,
  GameServerGroupStatus_ACTIVE,
  GameServerGroupStatus_DELETED,
  GameServerGroupStatus_DELETE_SCHEDULED,
  GameServerGroupStatus_DELETING,
  GameServerGroupStatus_ERROR,
  GameServerGroupStatus_NEW,
  GameServerGroupStatus'
  #-}
