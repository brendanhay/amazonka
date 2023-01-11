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
-- Module      : Amazonka.Route53Domains.Types.DomainAvailability
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53Domains.Types.DomainAvailability
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype DomainAvailability = DomainAvailability'
  { fromDomainAvailability ::
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
