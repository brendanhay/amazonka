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
-- Module      : Network.AWS.EC2.Types.Ipv6SupportValue
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.Ipv6SupportValue
  ( Ipv6SupportValue
      ( ..,
        Ipv6SupportValue_Disable,
        Ipv6SupportValue_Enable
      ),
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal

newtype Ipv6SupportValue = Ipv6SupportValue'
  { fromIpv6SupportValue ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
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

pattern Ipv6SupportValue_Disable :: Ipv6SupportValue
pattern Ipv6SupportValue_Disable = Ipv6SupportValue' "disable"

pattern Ipv6SupportValue_Enable :: Ipv6SupportValue
pattern Ipv6SupportValue_Enable = Ipv6SupportValue' "enable"

{-# COMPLETE
  Ipv6SupportValue_Disable,
  Ipv6SupportValue_Enable,
  Ipv6SupportValue'
  #-}
