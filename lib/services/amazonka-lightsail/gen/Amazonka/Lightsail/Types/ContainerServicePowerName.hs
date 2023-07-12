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
-- Module      : Amazonka.Lightsail.Types.ContainerServicePowerName
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lightsail.Types.ContainerServicePowerName
  ( ContainerServicePowerName
      ( ..,
        ContainerServicePowerName_Large,
        ContainerServicePowerName_Medium,
        ContainerServicePowerName_Micro,
        ContainerServicePowerName_Nano,
        ContainerServicePowerName_Small,
        ContainerServicePowerName_Xlarge
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ContainerServicePowerName = ContainerServicePowerName'
  { fromContainerServicePowerName ::
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

pattern ContainerServicePowerName_Large :: ContainerServicePowerName
pattern ContainerServicePowerName_Large = ContainerServicePowerName' "large"

pattern ContainerServicePowerName_Medium :: ContainerServicePowerName
pattern ContainerServicePowerName_Medium = ContainerServicePowerName' "medium"

pattern ContainerServicePowerName_Micro :: ContainerServicePowerName
pattern ContainerServicePowerName_Micro = ContainerServicePowerName' "micro"

pattern ContainerServicePowerName_Nano :: ContainerServicePowerName
pattern ContainerServicePowerName_Nano = ContainerServicePowerName' "nano"

pattern ContainerServicePowerName_Small :: ContainerServicePowerName
pattern ContainerServicePowerName_Small = ContainerServicePowerName' "small"

pattern ContainerServicePowerName_Xlarge :: ContainerServicePowerName
pattern ContainerServicePowerName_Xlarge = ContainerServicePowerName' "xlarge"

{-# COMPLETE
  ContainerServicePowerName_Large,
  ContainerServicePowerName_Medium,
  ContainerServicePowerName_Micro,
  ContainerServicePowerName_Nano,
  ContainerServicePowerName_Small,
  ContainerServicePowerName_Xlarge,
  ContainerServicePowerName'
  #-}
