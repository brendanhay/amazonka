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
-- Module      : Amazonka.SNS.Types.RouteType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SNS.Types.RouteType
  ( RouteType
      ( ..,
        RouteType_Premium,
        RouteType_Promotional,
        RouteType_Transactional
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

-- | Enum listing out all supported route types. The following enum values
-- are supported. 1. Transactional : Non-marketing traffic 2. Promotional :
-- Marketing 3. Premium : Premium routes for OTP delivery to the carriers
newtype RouteType = RouteType'
  { fromRouteType ::
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

pattern RouteType_Premium :: RouteType
pattern RouteType_Premium = RouteType' "Premium"

pattern RouteType_Promotional :: RouteType
pattern RouteType_Promotional = RouteType' "Promotional"

pattern RouteType_Transactional :: RouteType
pattern RouteType_Transactional = RouteType' "Transactional"

{-# COMPLETE
  RouteType_Premium,
  RouteType_Promotional,
  RouteType_Transactional,
  RouteType'
  #-}
