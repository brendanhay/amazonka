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
-- Module      : Network.AWS.ElasticSearch.Types.RollbackOnDisable
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.RollbackOnDisable
  ( RollbackOnDisable
      ( ..,
        RollbackOnDisable_DEFAULT_ROLLBACK,
        RollbackOnDisable_NO_ROLLBACK
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

-- | Specifies the rollback state while disabling Auto-Tune for the domain.
-- Valid values are NO_ROLLBACK, DEFAULT_ROLLBACK.
newtype RollbackOnDisable = RollbackOnDisable'
  { fromRollbackOnDisable ::
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

pattern RollbackOnDisable_DEFAULT_ROLLBACK :: RollbackOnDisable
pattern RollbackOnDisable_DEFAULT_ROLLBACK = RollbackOnDisable' "DEFAULT_ROLLBACK"

pattern RollbackOnDisable_NO_ROLLBACK :: RollbackOnDisable
pattern RollbackOnDisable_NO_ROLLBACK = RollbackOnDisable' "NO_ROLLBACK"

{-# COMPLETE
  RollbackOnDisable_DEFAULT_ROLLBACK,
  RollbackOnDisable_NO_ROLLBACK,
  RollbackOnDisable'
  #-}
