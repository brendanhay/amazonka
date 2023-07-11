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
-- Module      : Amazonka.Glue.Types.UpdateCatalogBehavior
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.UpdateCatalogBehavior
  ( UpdateCatalogBehavior
      ( ..,
        UpdateCatalogBehavior_LOG,
        UpdateCatalogBehavior_UPDATE_IN_DATABASE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype UpdateCatalogBehavior = UpdateCatalogBehavior'
  { fromUpdateCatalogBehavior ::
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

pattern UpdateCatalogBehavior_LOG :: UpdateCatalogBehavior
pattern UpdateCatalogBehavior_LOG = UpdateCatalogBehavior' "LOG"

pattern UpdateCatalogBehavior_UPDATE_IN_DATABASE :: UpdateCatalogBehavior
pattern UpdateCatalogBehavior_UPDATE_IN_DATABASE = UpdateCatalogBehavior' "UPDATE_IN_DATABASE"

{-# COMPLETE
  UpdateCatalogBehavior_LOG,
  UpdateCatalogBehavior_UPDATE_IN_DATABASE,
  UpdateCatalogBehavior'
  #-}
