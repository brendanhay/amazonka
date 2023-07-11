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
-- Module      : Amazonka.MacieV2.Types.ScopeFilterKey
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.ScopeFilterKey
  ( ScopeFilterKey
      ( ..,
        ScopeFilterKey_OBJECT_EXTENSION,
        ScopeFilterKey_OBJECT_KEY,
        ScopeFilterKey_OBJECT_LAST_MODIFIED_DATE,
        ScopeFilterKey_OBJECT_SIZE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The property to use in a condition that determines whether an S3 object
-- is included or excluded from a classification job. Valid values are:
newtype ScopeFilterKey = ScopeFilterKey'
  { fromScopeFilterKey ::
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

pattern ScopeFilterKey_OBJECT_EXTENSION :: ScopeFilterKey
pattern ScopeFilterKey_OBJECT_EXTENSION = ScopeFilterKey' "OBJECT_EXTENSION"

pattern ScopeFilterKey_OBJECT_KEY :: ScopeFilterKey
pattern ScopeFilterKey_OBJECT_KEY = ScopeFilterKey' "OBJECT_KEY"

pattern ScopeFilterKey_OBJECT_LAST_MODIFIED_DATE :: ScopeFilterKey
pattern ScopeFilterKey_OBJECT_LAST_MODIFIED_DATE = ScopeFilterKey' "OBJECT_LAST_MODIFIED_DATE"

pattern ScopeFilterKey_OBJECT_SIZE :: ScopeFilterKey
pattern ScopeFilterKey_OBJECT_SIZE = ScopeFilterKey' "OBJECT_SIZE"

{-# COMPLETE
  ScopeFilterKey_OBJECT_EXTENSION,
  ScopeFilterKey_OBJECT_KEY,
  ScopeFilterKey_OBJECT_LAST_MODIFIED_DATE,
  ScopeFilterKey_OBJECT_SIZE,
  ScopeFilterKey'
  #-}
