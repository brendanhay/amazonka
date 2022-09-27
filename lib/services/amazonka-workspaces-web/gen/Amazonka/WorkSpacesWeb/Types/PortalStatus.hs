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
-- Module      : Amazonka.WorkSpacesWeb.Types.PortalStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkSpacesWeb.Types.PortalStatus
  ( PortalStatus
      ( ..,
        PortalStatus_Active,
        PortalStatus_Incomplete,
        PortalStatus_Pending
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype PortalStatus = PortalStatus'
  { fromPortalStatus ::
      Core.Text
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
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
    )

pattern PortalStatus_Active :: PortalStatus
pattern PortalStatus_Active = PortalStatus' "Active"

pattern PortalStatus_Incomplete :: PortalStatus
pattern PortalStatus_Incomplete = PortalStatus' "Incomplete"

pattern PortalStatus_Pending :: PortalStatus
pattern PortalStatus_Pending = PortalStatus' "Pending"

{-# COMPLETE
  PortalStatus_Active,
  PortalStatus_Incomplete,
  PortalStatus_Pending,
  PortalStatus'
  #-}
