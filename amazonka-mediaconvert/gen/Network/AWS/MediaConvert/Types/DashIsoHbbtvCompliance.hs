{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
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

import qualified Network.AWS.Prelude as Prelude

-- | Supports HbbTV specification as indicated
newtype DashIsoHbbtvCompliance = DashIsoHbbtvCompliance'
  { fromDashIsoHbbtvCompliance ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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
