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
-- Module      : Network.AWS.EC2.Types.VpnEcmpSupportValue
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.VpnEcmpSupportValue
  ( VpnEcmpSupportValue
      ( ..,
        VpnEcmpSupportValue_Disable,
        VpnEcmpSupportValue_Enable
      ),
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import qualified Network.AWS.Prelude as Prelude

newtype VpnEcmpSupportValue = VpnEcmpSupportValue'
  { fromVpnEcmpSupportValue ::
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

pattern VpnEcmpSupportValue_Disable :: VpnEcmpSupportValue
pattern VpnEcmpSupportValue_Disable = VpnEcmpSupportValue' "disable"

pattern VpnEcmpSupportValue_Enable :: VpnEcmpSupportValue
pattern VpnEcmpSupportValue_Enable = VpnEcmpSupportValue' "enable"

{-# COMPLETE
  VpnEcmpSupportValue_Disable,
  VpnEcmpSupportValue_Enable,
  VpnEcmpSupportValue'
  #-}
