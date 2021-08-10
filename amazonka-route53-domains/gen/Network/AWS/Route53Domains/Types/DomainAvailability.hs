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
-- Module      : Network.AWS.Route53Domains.Types.DomainAvailability
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53Domains.Types.DomainAvailability
  ( DomainAvailability
      ( ..,
        DomainAvailability_AVAILABLE,
        DomainAvailability_AVAILABLE_PREORDER,
        DomainAvailability_AVAILABLE_RESERVED,
        DomainAvailability_DONT_KNOW,
        DomainAvailability_RESERVED,
        DomainAvailability_UNAVAILABLE,
        DomainAvailability_UNAVAILABLE_PREMIUM,
        DomainAvailability_UNAVAILABLE_RESTRICTED
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype DomainAvailability = DomainAvailability'
  { fromDomainAvailability ::
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

pattern DomainAvailability_AVAILABLE :: DomainAvailability
pattern DomainAvailability_AVAILABLE = DomainAvailability' "AVAILABLE"

pattern DomainAvailability_AVAILABLE_PREORDER :: DomainAvailability
pattern DomainAvailability_AVAILABLE_PREORDER = DomainAvailability' "AVAILABLE_PREORDER"

pattern DomainAvailability_AVAILABLE_RESERVED :: DomainAvailability
pattern DomainAvailability_AVAILABLE_RESERVED = DomainAvailability' "AVAILABLE_RESERVED"

pattern DomainAvailability_DONT_KNOW :: DomainAvailability
pattern DomainAvailability_DONT_KNOW = DomainAvailability' "DONT_KNOW"

pattern DomainAvailability_RESERVED :: DomainAvailability
pattern DomainAvailability_RESERVED = DomainAvailability' "RESERVED"

pattern DomainAvailability_UNAVAILABLE :: DomainAvailability
pattern DomainAvailability_UNAVAILABLE = DomainAvailability' "UNAVAILABLE"

pattern DomainAvailability_UNAVAILABLE_PREMIUM :: DomainAvailability
pattern DomainAvailability_UNAVAILABLE_PREMIUM = DomainAvailability' "UNAVAILABLE_PREMIUM"

pattern DomainAvailability_UNAVAILABLE_RESTRICTED :: DomainAvailability
pattern DomainAvailability_UNAVAILABLE_RESTRICTED = DomainAvailability' "UNAVAILABLE_RESTRICTED"

{-# COMPLETE
  DomainAvailability_AVAILABLE,
  DomainAvailability_AVAILABLE_PREORDER,
  DomainAvailability_AVAILABLE_RESERVED,
  DomainAvailability_DONT_KNOW,
  DomainAvailability_RESERVED,
  DomainAvailability_UNAVAILABLE,
  DomainAvailability_UNAVAILABLE_PREMIUM,
  DomainAvailability_UNAVAILABLE_RESTRICTED,
  DomainAvailability'
  #-}
