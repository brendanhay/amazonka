{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexRuntime.Types.DialogActionType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.LexRuntime.Types.DialogActionType
  ( DialogActionType
    ( DialogActionType'
    , DialogActionTypeElicitIntent
    , DialogActionTypeConfirmIntent
    , DialogActionTypeElicitSlot
    , DialogActionTypeClose
    , DialogActionTypeDelegate
    , fromDialogActionType
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype DialogActionType = DialogActionType'{fromDialogActionType
                                             :: Core.Text}
                             deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                             Core.Generic)
                             deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                               Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                               Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                               Core.FromText, Core.ToByteString, Core.ToQuery,
                                               Core.ToHeader)

pattern DialogActionTypeElicitIntent :: DialogActionType
pattern DialogActionTypeElicitIntent = DialogActionType' "ElicitIntent"

pattern DialogActionTypeConfirmIntent :: DialogActionType
pattern DialogActionTypeConfirmIntent = DialogActionType' "ConfirmIntent"

pattern DialogActionTypeElicitSlot :: DialogActionType
pattern DialogActionTypeElicitSlot = DialogActionType' "ElicitSlot"

pattern DialogActionTypeClose :: DialogActionType
pattern DialogActionTypeClose = DialogActionType' "Close"

pattern DialogActionTypeDelegate :: DialogActionType
pattern DialogActionTypeDelegate = DialogActionType' "Delegate"

{-# COMPLETE 
  DialogActionTypeElicitIntent,

  DialogActionTypeConfirmIntent,

  DialogActionTypeElicitSlot,

  DialogActionTypeClose,

  DialogActionTypeDelegate,
  DialogActionType'
  #-}
