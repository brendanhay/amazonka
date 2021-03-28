{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.WriteForwardingStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.RDS.Types.WriteForwardingStatus
  ( WriteForwardingStatus
    ( WriteForwardingStatus'
    , WriteForwardingStatusEnabled
    , WriteForwardingStatusDisabled
    , WriteForwardingStatusEnabling
    , WriteForwardingStatusDisabling
    , WriteForwardingStatusUnknown
    , fromWriteForwardingStatus
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype WriteForwardingStatus = WriteForwardingStatus'{fromWriteForwardingStatus
                                                       :: Core.Text}
                                  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                  Core.Generic)
                                  deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                    Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                    Core.FromJSON, Core.ToXML, Core.FromXML,
                                                    Core.ToText, Core.FromText, Core.ToByteString,
                                                    Core.ToQuery, Core.ToHeader)

pattern WriteForwardingStatusEnabled :: WriteForwardingStatus
pattern WriteForwardingStatusEnabled = WriteForwardingStatus' "enabled"

pattern WriteForwardingStatusDisabled :: WriteForwardingStatus
pattern WriteForwardingStatusDisabled = WriteForwardingStatus' "disabled"

pattern WriteForwardingStatusEnabling :: WriteForwardingStatus
pattern WriteForwardingStatusEnabling = WriteForwardingStatus' "enabling"

pattern WriteForwardingStatusDisabling :: WriteForwardingStatus
pattern WriteForwardingStatusDisabling = WriteForwardingStatus' "disabling"

pattern WriteForwardingStatusUnknown :: WriteForwardingStatus
pattern WriteForwardingStatusUnknown = WriteForwardingStatus' "unknown"

{-# COMPLETE 
  WriteForwardingStatusEnabled,

  WriteForwardingStatusDisabled,

  WriteForwardingStatusEnabling,

  WriteForwardingStatusDisabling,

  WriteForwardingStatusUnknown,
  WriteForwardingStatus'
  #-}
