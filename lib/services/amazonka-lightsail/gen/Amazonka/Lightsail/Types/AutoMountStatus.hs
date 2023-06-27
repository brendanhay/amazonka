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
-- Module      : Amazonka.Lightsail.Types.AutoMountStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lightsail.Types.AutoMountStatus
  ( AutoMountStatus
      ( ..,
        AutoMountStatus_Failed,
        AutoMountStatus_Mounted,
        AutoMountStatus_NotMounted,
        AutoMountStatus_Pending
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype AutoMountStatus = AutoMountStatus'
  { fromAutoMountStatus ::
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

pattern AutoMountStatus_Failed :: AutoMountStatus
pattern AutoMountStatus_Failed = AutoMountStatus' "Failed"

pattern AutoMountStatus_Mounted :: AutoMountStatus
pattern AutoMountStatus_Mounted = AutoMountStatus' "Mounted"

pattern AutoMountStatus_NotMounted :: AutoMountStatus
pattern AutoMountStatus_NotMounted = AutoMountStatus' "NotMounted"

pattern AutoMountStatus_Pending :: AutoMountStatus
pattern AutoMountStatus_Pending = AutoMountStatus' "Pending"

{-# COMPLETE
  AutoMountStatus_Failed,
  AutoMountStatus_Mounted,
  AutoMountStatus_NotMounted,
  AutoMountStatus_Pending,
  AutoMountStatus'
  #-}
