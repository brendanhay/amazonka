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
-- Module      : Amazonka.SageMaker.Types.DeviceSubsetType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.DeviceSubsetType
  ( DeviceSubsetType
      ( ..,
        DeviceSubsetType_NAMECONTAINS,
        DeviceSubsetType_PERCENTAGE,
        DeviceSubsetType_SELECTION
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype DeviceSubsetType = DeviceSubsetType'
  { fromDeviceSubsetType ::
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

pattern DeviceSubsetType_NAMECONTAINS :: DeviceSubsetType
pattern DeviceSubsetType_NAMECONTAINS = DeviceSubsetType' "NAMECONTAINS"

pattern DeviceSubsetType_PERCENTAGE :: DeviceSubsetType
pattern DeviceSubsetType_PERCENTAGE = DeviceSubsetType' "PERCENTAGE"

pattern DeviceSubsetType_SELECTION :: DeviceSubsetType
pattern DeviceSubsetType_SELECTION = DeviceSubsetType' "SELECTION"

{-# COMPLETE
  DeviceSubsetType_NAMECONTAINS,
  DeviceSubsetType_PERCENTAGE,
  DeviceSubsetType_SELECTION,
  DeviceSubsetType'
  #-}
