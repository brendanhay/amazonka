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
-- Module      : Amazonka.MacieV2.Types.ManagedDataIdentifierSelector
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.ManagedDataIdentifierSelector
  ( ManagedDataIdentifierSelector
      ( ..,
        ManagedDataIdentifierSelector_ALL,
        ManagedDataIdentifierSelector_EXCLUDE,
        ManagedDataIdentifierSelector_INCLUDE,
        ManagedDataIdentifierSelector_NONE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

-- | The selection type that determines which managed data identifiers a
-- classification job uses to analyze data. Valid values are:
newtype ManagedDataIdentifierSelector = ManagedDataIdentifierSelector'
  { fromManagedDataIdentifierSelector ::
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

pattern ManagedDataIdentifierSelector_ALL :: ManagedDataIdentifierSelector
pattern ManagedDataIdentifierSelector_ALL = ManagedDataIdentifierSelector' "ALL"

pattern ManagedDataIdentifierSelector_EXCLUDE :: ManagedDataIdentifierSelector
pattern ManagedDataIdentifierSelector_EXCLUDE = ManagedDataIdentifierSelector' "EXCLUDE"

pattern ManagedDataIdentifierSelector_INCLUDE :: ManagedDataIdentifierSelector
pattern ManagedDataIdentifierSelector_INCLUDE = ManagedDataIdentifierSelector' "INCLUDE"

pattern ManagedDataIdentifierSelector_NONE :: ManagedDataIdentifierSelector
pattern ManagedDataIdentifierSelector_NONE = ManagedDataIdentifierSelector' "NONE"

{-# COMPLETE
  ManagedDataIdentifierSelector_ALL,
  ManagedDataIdentifierSelector_EXCLUDE,
  ManagedDataIdentifierSelector_INCLUDE,
  ManagedDataIdentifierSelector_NONE,
  ManagedDataIdentifierSelector'
  #-}
