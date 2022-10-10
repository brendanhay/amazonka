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
-- Module      : Amazonka.FSx.Types.AliasLifecycle
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FSx.Types.AliasLifecycle
  ( AliasLifecycle
      ( ..,
        AliasLifecycle_AVAILABLE,
        AliasLifecycle_CREATE_FAILED,
        AliasLifecycle_CREATING,
        AliasLifecycle_DELETE_FAILED,
        AliasLifecycle_DELETING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype AliasLifecycle = AliasLifecycle'
  { fromAliasLifecycle ::
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

pattern AliasLifecycle_AVAILABLE :: AliasLifecycle
pattern AliasLifecycle_AVAILABLE = AliasLifecycle' "AVAILABLE"

pattern AliasLifecycle_CREATE_FAILED :: AliasLifecycle
pattern AliasLifecycle_CREATE_FAILED = AliasLifecycle' "CREATE_FAILED"

pattern AliasLifecycle_CREATING :: AliasLifecycle
pattern AliasLifecycle_CREATING = AliasLifecycle' "CREATING"

pattern AliasLifecycle_DELETE_FAILED :: AliasLifecycle
pattern AliasLifecycle_DELETE_FAILED = AliasLifecycle' "DELETE_FAILED"

pattern AliasLifecycle_DELETING :: AliasLifecycle
pattern AliasLifecycle_DELETING = AliasLifecycle' "DELETING"

{-# COMPLETE
  AliasLifecycle_AVAILABLE,
  AliasLifecycle_CREATE_FAILED,
  AliasLifecycle_CREATING,
  AliasLifecycle_DELETE_FAILED,
  AliasLifecycle_DELETING,
  AliasLifecycle'
  #-}
