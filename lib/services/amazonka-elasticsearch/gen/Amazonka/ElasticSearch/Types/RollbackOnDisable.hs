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
-- Module      : Amazonka.ElasticSearch.Types.RollbackOnDisable
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElasticSearch.Types.RollbackOnDisable
  ( RollbackOnDisable
      ( ..,
        RollbackOnDisable_DEFAULT_ROLLBACK,
        RollbackOnDisable_NO_ROLLBACK
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies the rollback state while disabling Auto-Tune for the domain.
-- Valid values are NO_ROLLBACK, DEFAULT_ROLLBACK.
newtype RollbackOnDisable = RollbackOnDisable'
  { fromRollbackOnDisable ::
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

pattern RollbackOnDisable_DEFAULT_ROLLBACK :: RollbackOnDisable
pattern RollbackOnDisable_DEFAULT_ROLLBACK = RollbackOnDisable' "DEFAULT_ROLLBACK"

pattern RollbackOnDisable_NO_ROLLBACK :: RollbackOnDisable
pattern RollbackOnDisable_NO_ROLLBACK = RollbackOnDisable' "NO_ROLLBACK"

{-# COMPLETE
  RollbackOnDisable_DEFAULT_ROLLBACK,
  RollbackOnDisable_NO_ROLLBACK,
  RollbackOnDisable'
  #-}
