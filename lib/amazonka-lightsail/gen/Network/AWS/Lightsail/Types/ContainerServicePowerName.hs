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
        Nano,
        Micro,
        Small,
        Medium,
        Large,
        XLarge
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ContainerServicePowerName = ContainerServicePowerName' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern Nano :: ContainerServicePowerName
pattern Nano = ContainerServicePowerName' "nano"

pattern Micro :: ContainerServicePowerName
pattern Micro = ContainerServicePowerName' "micro"

pattern Small :: ContainerServicePowerName
pattern Small = ContainerServicePowerName' "small"

pattern Medium :: ContainerServicePowerName
pattern Medium = ContainerServicePowerName' "medium"

pattern Large :: ContainerServicePowerName
pattern Large = ContainerServicePowerName' "large"

pattern XLarge :: ContainerServicePowerName
pattern XLarge = ContainerServicePowerName' "xlarge"

{-# COMPLETE
  Nano,
  Micro,
  Small,
  Medium,
  Large,
  XLarge,
  ContainerServicePowerName'
  #-}
