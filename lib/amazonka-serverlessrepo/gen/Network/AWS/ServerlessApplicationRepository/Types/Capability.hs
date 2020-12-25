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
        CapabilityCapabilityIam,
        CapabilityCapabilityNamedIam,
        CapabilityCapabilityAutoExpand,
        CapabilityCapabilityResourcePolicy,
        fromCapability
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | Values that must be specified in order to deploy some applications.
newtype Capability = Capability' {fromCapability :: Core.Text}
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

pattern CapabilityCapabilityIam :: Capability
pattern CapabilityCapabilityIam = Capability' "CAPABILITY_IAM"

pattern CapabilityCapabilityNamedIam :: Capability
pattern CapabilityCapabilityNamedIam = Capability' "CAPABILITY_NAMED_IAM"

pattern CapabilityCapabilityAutoExpand :: Capability
pattern CapabilityCapabilityAutoExpand = Capability' "CAPABILITY_AUTO_EXPAND"

pattern CapabilityCapabilityResourcePolicy :: Capability
pattern CapabilityCapabilityResourcePolicy = Capability' "CAPABILITY_RESOURCE_POLICY"

{-# COMPLETE
  CapabilityCapabilityIam,
  CapabilityCapabilityNamedIam,
  CapabilityCapabilityAutoExpand,
  CapabilityCapabilityResourcePolicy,
  Capability'
  #-}
