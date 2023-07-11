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
-- Module      : Amazonka.CustomerProfiles.Types.ConflictResolvingModel
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CustomerProfiles.Types.ConflictResolvingModel
  ( ConflictResolvingModel
      ( ..,
        ConflictResolvingModel_RECENCY,
        ConflictResolvingModel_SOURCE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ConflictResolvingModel = ConflictResolvingModel'
  { fromConflictResolvingModel ::
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

pattern ConflictResolvingModel_RECENCY :: ConflictResolvingModel
pattern ConflictResolvingModel_RECENCY = ConflictResolvingModel' "RECENCY"

pattern ConflictResolvingModel_SOURCE :: ConflictResolvingModel
pattern ConflictResolvingModel_SOURCE = ConflictResolvingModel' "SOURCE"

{-# COMPLETE
  ConflictResolvingModel_RECENCY,
  ConflictResolvingModel_SOURCE,
  ConflictResolvingModel'
  #-}
