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
-- Module      : Network.AWS.MediaConvert.Types.DashIsoHbbtvCompliance
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.DashIsoHbbtvCompliance
  ( DashIsoHbbtvCompliance
      ( ..,
        DashIsoHbbtvCompliance_HBBTV_1_5,
        DashIsoHbbtvCompliance_NONE
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | Supports HbbTV specification as indicated
newtype DashIsoHbbtvCompliance = DashIsoHbbtvCompliance'
  { fromDashIsoHbbtvCompliance ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
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

pattern DashIsoHbbtvCompliance_HBBTV_1_5 :: DashIsoHbbtvCompliance
pattern DashIsoHbbtvCompliance_HBBTV_1_5 = DashIsoHbbtvCompliance' "HBBTV_1_5"

pattern DashIsoHbbtvCompliance_NONE :: DashIsoHbbtvCompliance
pattern DashIsoHbbtvCompliance_NONE = DashIsoHbbtvCompliance' "NONE"

{-# COMPLETE
  DashIsoHbbtvCompliance_HBBTV_1_5,
  DashIsoHbbtvCompliance_NONE,
  DashIsoHbbtvCompliance'
  #-}
