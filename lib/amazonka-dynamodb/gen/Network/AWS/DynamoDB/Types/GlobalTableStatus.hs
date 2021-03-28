{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.GlobalTableStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DynamoDB.Types.GlobalTableStatus
  ( GlobalTableStatus
    ( GlobalTableStatus'
    , GlobalTableStatusCreating
    , GlobalTableStatusActive
    , GlobalTableStatusDeleting
    , GlobalTableStatusUpdating
    , fromGlobalTableStatus
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype GlobalTableStatus = GlobalTableStatus'{fromGlobalTableStatus
                                               :: Core.Text}
                              deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                              Core.Generic)
                              deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                Core.FromJSON, Core.ToXML, Core.FromXML,
                                                Core.ToText, Core.FromText, Core.ToByteString,
                                                Core.ToQuery, Core.ToHeader)

pattern GlobalTableStatusCreating :: GlobalTableStatus
pattern GlobalTableStatusCreating = GlobalTableStatus' "CREATING"

pattern GlobalTableStatusActive :: GlobalTableStatus
pattern GlobalTableStatusActive = GlobalTableStatus' "ACTIVE"

pattern GlobalTableStatusDeleting :: GlobalTableStatus
pattern GlobalTableStatusDeleting = GlobalTableStatus' "DELETING"

pattern GlobalTableStatusUpdating :: GlobalTableStatus
pattern GlobalTableStatusUpdating = GlobalTableStatus' "UPDATING"

{-# COMPLETE 
  GlobalTableStatusCreating,

  GlobalTableStatusActive,

  GlobalTableStatusDeleting,

  GlobalTableStatusUpdating,
  GlobalTableStatus'
  #-}
