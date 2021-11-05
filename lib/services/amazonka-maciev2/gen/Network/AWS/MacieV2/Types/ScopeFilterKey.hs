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
-- Module      : Network.AWS.MacieV2.Types.ScopeFilterKey
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MacieV2.Types.ScopeFilterKey
  ( ScopeFilterKey
      ( ..,
        ScopeFilterKey_OBJECT_EXTENSION,
        ScopeFilterKey_OBJECT_KEY,
        ScopeFilterKey_OBJECT_LAST_MODIFIED_DATE,
        ScopeFilterKey_OBJECT_SIZE
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

-- | The property to use in a condition that determines whether an S3 object
-- is included or excluded from a classification job. Valid values are:
newtype ScopeFilterKey = ScopeFilterKey'
  { fromScopeFilterKey ::
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
