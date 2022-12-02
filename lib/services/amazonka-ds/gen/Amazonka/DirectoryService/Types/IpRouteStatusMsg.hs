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
-- Module      : Amazonka.DirectoryService.Types.IpRouteStatusMsg
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DirectoryService.Types.IpRouteStatusMsg
  ( IpRouteStatusMsg
      ( ..,
        IpRouteStatusMsg_AddFailed,
        IpRouteStatusMsg_Added,
        IpRouteStatusMsg_Adding,
        IpRouteStatusMsg_RemoveFailed,
        IpRouteStatusMsg_Removed,
        IpRouteStatusMsg_Removing
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype IpRouteStatusMsg = IpRouteStatusMsg'
  { fromIpRouteStatusMsg ::
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

pattern IpRouteStatusMsg_AddFailed :: IpRouteStatusMsg
pattern IpRouteStatusMsg_AddFailed = IpRouteStatusMsg' "AddFailed"

pattern IpRouteStatusMsg_Added :: IpRouteStatusMsg
pattern IpRouteStatusMsg_Added = IpRouteStatusMsg' "Added"

pattern IpRouteStatusMsg_Adding :: IpRouteStatusMsg
pattern IpRouteStatusMsg_Adding = IpRouteStatusMsg' "Adding"

pattern IpRouteStatusMsg_RemoveFailed :: IpRouteStatusMsg
pattern IpRouteStatusMsg_RemoveFailed = IpRouteStatusMsg' "RemoveFailed"

pattern IpRouteStatusMsg_Removed :: IpRouteStatusMsg
pattern IpRouteStatusMsg_Removed = IpRouteStatusMsg' "Removed"

pattern IpRouteStatusMsg_Removing :: IpRouteStatusMsg
pattern IpRouteStatusMsg_Removing = IpRouteStatusMsg' "Removing"

{-# COMPLETE
  IpRouteStatusMsg_AddFailed,
  IpRouteStatusMsg_Added,
  IpRouteStatusMsg_Adding,
  IpRouteStatusMsg_RemoveFailed,
  IpRouteStatusMsg_Removed,
  IpRouteStatusMsg_Removing,
  IpRouteStatusMsg'
  #-}
