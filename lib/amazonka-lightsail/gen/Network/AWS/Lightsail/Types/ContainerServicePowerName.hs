{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.ContainerServicePowerName
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.ContainerServicePowerName
  ( ContainerServicePowerName
      ( ContainerServicePowerName',
        ContainerServicePowerNameNano,
        ContainerServicePowerNameMicro,
        ContainerServicePowerNameSmall,
        ContainerServicePowerNameMedium,
        ContainerServicePowerNameLarge,
        ContainerServicePowerNameXlarge,
        fromContainerServicePowerName
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype ContainerServicePowerName = ContainerServicePowerName'
  { fromContainerServicePowerName ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern ContainerServicePowerNameNano :: ContainerServicePowerName
pattern ContainerServicePowerNameNano = ContainerServicePowerName' "nano"

pattern ContainerServicePowerNameMicro :: ContainerServicePowerName
pattern ContainerServicePowerNameMicro = ContainerServicePowerName' "micro"

pattern ContainerServicePowerNameSmall :: ContainerServicePowerName
pattern ContainerServicePowerNameSmall = ContainerServicePowerName' "small"

pattern ContainerServicePowerNameMedium :: ContainerServicePowerName
pattern ContainerServicePowerNameMedium = ContainerServicePowerName' "medium"

pattern ContainerServicePowerNameLarge :: ContainerServicePowerName
pattern ContainerServicePowerNameLarge = ContainerServicePowerName' "large"

pattern ContainerServicePowerNameXlarge :: ContainerServicePowerName
pattern ContainerServicePowerNameXlarge = ContainerServicePowerName' "xlarge"

{-# COMPLETE
  ContainerServicePowerNameNano,
  ContainerServicePowerNameMicro,
  ContainerServicePowerNameSmall,
  ContainerServicePowerNameMedium,
  ContainerServicePowerNameLarge,
  ContainerServicePowerNameXlarge,
  ContainerServicePowerName'
  #-}
