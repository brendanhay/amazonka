{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.UpdateActionType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.UpdateActionType
  ( UpdateActionType
      ( UpdateActionType',
        UpdateActionTypeCreateOrUpdate,
        UpdateActionTypeDelete,
        fromUpdateActionType
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype UpdateActionType = UpdateActionType'
  { fromUpdateActionType ::
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

pattern UpdateActionTypeCreateOrUpdate :: UpdateActionType
pattern UpdateActionTypeCreateOrUpdate = UpdateActionType' "CREATE_OR_UPDATE"

pattern UpdateActionTypeDelete :: UpdateActionType
pattern UpdateActionTypeDelete = UpdateActionType' "DELETE"

{-# COMPLETE
  UpdateActionTypeCreateOrUpdate,
  UpdateActionTypeDelete,
  UpdateActionType'
  #-}
