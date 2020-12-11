-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.DomainConfigurationStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.DomainConfigurationStatus
  ( DomainConfigurationStatus
      ( DomainConfigurationStatus',
        DCSDisabled,
        DCSEnabled
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype DomainConfigurationStatus = DomainConfigurationStatus' Lude.Text
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

pattern DCSDisabled :: DomainConfigurationStatus
pattern DCSDisabled = DomainConfigurationStatus' "DISABLED"

pattern DCSEnabled :: DomainConfigurationStatus
pattern DCSEnabled = DomainConfigurationStatus' "ENABLED"

{-# COMPLETE
  DCSDisabled,
  DCSEnabled,
  DomainConfigurationStatus'
  #-}
