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
-- Module      : Amazonka.CustomerProfiles.Types.StandardIdentifier
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CustomerProfiles.Types.StandardIdentifier
  ( StandardIdentifier
      ( ..,
        StandardIdentifier_ASSET,
        StandardIdentifier_CASE,
        StandardIdentifier_LOOKUP_ONLY,
        StandardIdentifier_NEW_ONLY,
        StandardIdentifier_ORDER,
        StandardIdentifier_PROFILE,
        StandardIdentifier_SECONDARY,
        StandardIdentifier_UNIQUE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype StandardIdentifier = StandardIdentifier'
  { fromStandardIdentifier ::
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

pattern StandardIdentifier_ASSET :: StandardIdentifier
pattern StandardIdentifier_ASSET = StandardIdentifier' "ASSET"

pattern StandardIdentifier_CASE :: StandardIdentifier
pattern StandardIdentifier_CASE = StandardIdentifier' "CASE"

pattern StandardIdentifier_LOOKUP_ONLY :: StandardIdentifier
pattern StandardIdentifier_LOOKUP_ONLY = StandardIdentifier' "LOOKUP_ONLY"

pattern StandardIdentifier_NEW_ONLY :: StandardIdentifier
pattern StandardIdentifier_NEW_ONLY = StandardIdentifier' "NEW_ONLY"

pattern StandardIdentifier_ORDER :: StandardIdentifier
pattern StandardIdentifier_ORDER = StandardIdentifier' "ORDER"

pattern StandardIdentifier_PROFILE :: StandardIdentifier
pattern StandardIdentifier_PROFILE = StandardIdentifier' "PROFILE"

pattern StandardIdentifier_SECONDARY :: StandardIdentifier
pattern StandardIdentifier_SECONDARY = StandardIdentifier' "SECONDARY"

pattern StandardIdentifier_UNIQUE :: StandardIdentifier
pattern StandardIdentifier_UNIQUE = StandardIdentifier' "UNIQUE"

{-# COMPLETE
  StandardIdentifier_ASSET,
  StandardIdentifier_CASE,
  StandardIdentifier_LOOKUP_ONLY,
  StandardIdentifier_NEW_ONLY,
  StandardIdentifier_ORDER,
  StandardIdentifier_PROFILE,
  StandardIdentifier_SECONDARY,
  StandardIdentifier_UNIQUE,
  StandardIdentifier'
  #-}
