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
-- Module      : Amazonka.CleanRooms.Types.MembershipStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CleanRooms.Types.MembershipStatus
  ( MembershipStatus
      ( ..,
        MembershipStatus_ACTIVE,
        MembershipStatus_COLLABORATION_DELETED,
        MembershipStatus_REMOVED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype MembershipStatus = MembershipStatus'
  { fromMembershipStatus ::
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

pattern MembershipStatus_ACTIVE :: MembershipStatus
pattern MembershipStatus_ACTIVE = MembershipStatus' "ACTIVE"

pattern MembershipStatus_COLLABORATION_DELETED :: MembershipStatus
pattern MembershipStatus_COLLABORATION_DELETED = MembershipStatus' "COLLABORATION_DELETED"

pattern MembershipStatus_REMOVED :: MembershipStatus
pattern MembershipStatus_REMOVED = MembershipStatus' "REMOVED"

{-# COMPLETE
  MembershipStatus_ACTIVE,
  MembershipStatus_COLLABORATION_DELETED,
  MembershipStatus_REMOVED,
  MembershipStatus'
  #-}
