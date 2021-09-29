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
-- Module      : Network.AWS.EC2.Types.LaunchTemplateInstanceMetadataProtocolIpv6
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LaunchTemplateInstanceMetadataProtocolIpv6
  ( LaunchTemplateInstanceMetadataProtocolIpv6
      ( ..,
        LaunchTemplateInstanceMetadataProtocolIpv6_Disabled,
        LaunchTemplateInstanceMetadataProtocolIpv6_Enabled
      ),
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import qualified Network.AWS.Prelude as Prelude

newtype LaunchTemplateInstanceMetadataProtocolIpv6 = LaunchTemplateInstanceMetadataProtocolIpv6'
  { fromLaunchTemplateInstanceMetadataProtocolIpv6 ::
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

pattern LaunchTemplateInstanceMetadataProtocolIpv6_Disabled :: LaunchTemplateInstanceMetadataProtocolIpv6
pattern LaunchTemplateInstanceMetadataProtocolIpv6_Disabled = LaunchTemplateInstanceMetadataProtocolIpv6' "disabled"

pattern LaunchTemplateInstanceMetadataProtocolIpv6_Enabled :: LaunchTemplateInstanceMetadataProtocolIpv6
pattern LaunchTemplateInstanceMetadataProtocolIpv6_Enabled = LaunchTemplateInstanceMetadataProtocolIpv6' "enabled"

{-# COMPLETE
  LaunchTemplateInstanceMetadataProtocolIpv6_Disabled,
  LaunchTemplateInstanceMetadataProtocolIpv6_Enabled,
  LaunchTemplateInstanceMetadataProtocolIpv6'
  #-}
