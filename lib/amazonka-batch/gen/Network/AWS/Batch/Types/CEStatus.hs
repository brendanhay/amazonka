{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.Types.CEStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Batch.Types.CEStatus
  ( CEStatus
      ( CEStatus',
        CEStatusCreating,
        CEStatusUpdating,
        CEStatusDeleting,
        CEStatusDeleted,
        CEStatusValid,
        CEStatusInvalid,
        fromCEStatus
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype CEStatus = CEStatus' {fromCEStatus :: Core.Text}
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

pattern CEStatusCreating :: CEStatus
pattern CEStatusCreating = CEStatus' "CREATING"

pattern CEStatusUpdating :: CEStatus
pattern CEStatusUpdating = CEStatus' "UPDATING"

pattern CEStatusDeleting :: CEStatus
pattern CEStatusDeleting = CEStatus' "DELETING"

pattern CEStatusDeleted :: CEStatus
pattern CEStatusDeleted = CEStatus' "DELETED"

pattern CEStatusValid :: CEStatus
pattern CEStatusValid = CEStatus' "VALID"

pattern CEStatusInvalid :: CEStatus
pattern CEStatusInvalid = CEStatus' "INVALID"

{-# COMPLETE
  CEStatusCreating,
  CEStatusUpdating,
  CEStatusDeleting,
  CEStatusDeleted,
  CEStatusValid,
  CEStatusInvalid,
  CEStatus'
  #-}
