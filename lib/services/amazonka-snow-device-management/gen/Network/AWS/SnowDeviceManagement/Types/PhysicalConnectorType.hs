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
-- Module      : Network.AWS.SnowDeviceManagement.Types.PhysicalConnectorType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SnowDeviceManagement.Types.PhysicalConnectorType
  ( PhysicalConnectorType
      ( ..,
        PhysicalConnectorType_QSFP,
        PhysicalConnectorType_RJ45,
        PhysicalConnectorType_RJ45_2,
        PhysicalConnectorType_SFP_PLUS,
        PhysicalConnectorType_WIFI
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype PhysicalConnectorType = PhysicalConnectorType'
  { fromPhysicalConnectorType ::
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

pattern PhysicalConnectorType_QSFP :: PhysicalConnectorType
pattern PhysicalConnectorType_QSFP = PhysicalConnectorType' "QSFP"

pattern PhysicalConnectorType_RJ45 :: PhysicalConnectorType
pattern PhysicalConnectorType_RJ45 = PhysicalConnectorType' "RJ45"

pattern PhysicalConnectorType_RJ45_2 :: PhysicalConnectorType
pattern PhysicalConnectorType_RJ45_2 = PhysicalConnectorType' "RJ45_2"

pattern PhysicalConnectorType_SFP_PLUS :: PhysicalConnectorType
pattern PhysicalConnectorType_SFP_PLUS = PhysicalConnectorType' "SFP_PLUS"

pattern PhysicalConnectorType_WIFI :: PhysicalConnectorType
pattern PhysicalConnectorType_WIFI = PhysicalConnectorType' "WIFI"

{-# COMPLETE
  PhysicalConnectorType_QSFP,
  PhysicalConnectorType_RJ45,
  PhysicalConnectorType_RJ45_2,
  PhysicalConnectorType_SFP_PLUS,
  PhysicalConnectorType_WIFI,
  PhysicalConnectorType'
  #-}
