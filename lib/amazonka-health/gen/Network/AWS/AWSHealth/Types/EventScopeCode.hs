{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AWSHealth.Types.EventScopeCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AWSHealth.Types.EventScopeCode
  ( EventScopeCode
    ( EventScopeCode'
    , EventScopeCodePublic
    , EventScopeCodeAccountSpecific
    , EventScopeCodeNone
    , fromEventScopeCode
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype EventScopeCode = EventScopeCode'{fromEventScopeCode ::
                                         Core.Text}
                           deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                           Core.Generic)
                           deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                             Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                             Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                             Core.FromText, Core.ToByteString, Core.ToQuery,
                                             Core.ToHeader)

pattern EventScopeCodePublic :: EventScopeCode
pattern EventScopeCodePublic = EventScopeCode' "PUBLIC"

pattern EventScopeCodeAccountSpecific :: EventScopeCode
pattern EventScopeCodeAccountSpecific = EventScopeCode' "ACCOUNT_SPECIFIC"

pattern EventScopeCodeNone :: EventScopeCode
pattern EventScopeCodeNone = EventScopeCode' "NONE"

{-# COMPLETE 
  EventScopeCodePublic,

  EventScopeCodeAccountSpecific,

  EventScopeCodeNone,
  EventScopeCode'
  #-}
