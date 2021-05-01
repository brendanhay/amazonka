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
-- Module      : Network.AWS.GameLift.Types.GameServerInstanceStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.GameServerInstanceStatus
  ( GameServerInstanceStatus
      ( ..,
        GameServerInstanceStatus_ACTIVE,
        GameServerInstanceStatus_DRAINING,
        GameServerInstanceStatus_SPOT_TERMINATING
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype GameServerInstanceStatus = GameServerInstanceStatus'
  { fromGameServerInstanceStatus ::
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

pattern GameServerInstanceStatus_ACTIVE :: GameServerInstanceStatus
pattern GameServerInstanceStatus_ACTIVE = GameServerInstanceStatus' "ACTIVE"

pattern GameServerInstanceStatus_DRAINING :: GameServerInstanceStatus
pattern GameServerInstanceStatus_DRAINING = GameServerInstanceStatus' "DRAINING"

pattern GameServerInstanceStatus_SPOT_TERMINATING :: GameServerInstanceStatus
pattern GameServerInstanceStatus_SPOT_TERMINATING = GameServerInstanceStatus' "SPOT_TERMINATING"

{-# COMPLETE
  GameServerInstanceStatus_ACTIVE,
  GameServerInstanceStatus_DRAINING,
  GameServerInstanceStatus_SPOT_TERMINATING,
  GameServerInstanceStatus'
  #-}
