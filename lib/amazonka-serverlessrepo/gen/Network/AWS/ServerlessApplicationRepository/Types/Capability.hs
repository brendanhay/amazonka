{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServerlessApplicationRepository.Types.Capability
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServerlessApplicationRepository.Types.Capability
  ( Capability
      ( Capability',
        CapabilityAutoExpand,
        CapabilityIAM,
        CapabilityNamedIAM,
        CapabilityResourcePolicy
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Values that must be specified in order to deploy some applications.
newtype Capability = Capability' Lude.Text
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

pattern CapabilityAutoExpand :: Capability
pattern CapabilityAutoExpand = Capability' "CAPABILITY_AUTO_EXPAND"

pattern CapabilityIAM :: Capability
pattern CapabilityIAM = Capability' "CAPABILITY_IAM"

pattern CapabilityNamedIAM :: Capability
pattern CapabilityNamedIAM = Capability' "CAPABILITY_NAMED_IAM"

pattern CapabilityResourcePolicy :: Capability
pattern CapabilityResourcePolicy = Capability' "CAPABILITY_RESOURCE_POLICY"

{-# COMPLETE
  CapabilityAutoExpand,
  CapabilityIAM,
  CapabilityNamedIAM,
  CapabilityResourcePolicy,
  Capability'
  #-}
