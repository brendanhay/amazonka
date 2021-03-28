{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.ActionType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.ActionType
  ( ActionType
    ( ActionType'
    , ActionTypePublish
    , ActionTypeSubscribe
    , ActionTypeReceive
    , ActionTypeConnect
    , fromActionType
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype ActionType = ActionType'{fromActionType :: Core.Text}
                       deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                       Core.Generic)
                       deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                         Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                         Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                         Core.FromText, Core.ToByteString, Core.ToQuery,
                                         Core.ToHeader)

pattern ActionTypePublish :: ActionType
pattern ActionTypePublish = ActionType' "PUBLISH"

pattern ActionTypeSubscribe :: ActionType
pattern ActionTypeSubscribe = ActionType' "SUBSCRIBE"

pattern ActionTypeReceive :: ActionType
pattern ActionTypeReceive = ActionType' "RECEIVE"

pattern ActionTypeConnect :: ActionType
pattern ActionTypeConnect = ActionType' "CONNECT"

{-# COMPLETE 
  ActionTypePublish,

  ActionTypeSubscribe,

  ActionTypeReceive,

  ActionTypeConnect,
  ActionType'
  #-}
