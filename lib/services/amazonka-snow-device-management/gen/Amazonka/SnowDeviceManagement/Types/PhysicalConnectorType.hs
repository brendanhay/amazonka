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
-- Module      : Amazonka.SnowDeviceManagement.Types.PhysicalConnectorType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SnowDeviceManagement.Types.PhysicalConnectorType
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype PhysicalConnectorType = PhysicalConnectorType'
  { fromPhysicalConnectorType ::
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
