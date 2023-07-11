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
-- Module      : Amazonka.WellArchitected.Types.DifferenceStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WellArchitected.Types.DifferenceStatus
  ( DifferenceStatus
      ( ..,
        DifferenceStatus_DELETED,
        DifferenceStatus_NEW,
        DifferenceStatus_UPDATED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype DifferenceStatus = DifferenceStatus'
  { fromDifferenceStatus ::
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

pattern DifferenceStatus_DELETED :: DifferenceStatus
pattern DifferenceStatus_DELETED = DifferenceStatus' "DELETED"

pattern DifferenceStatus_NEW :: DifferenceStatus
pattern DifferenceStatus_NEW = DifferenceStatus' "NEW"

pattern DifferenceStatus_UPDATED :: DifferenceStatus
pattern DifferenceStatus_UPDATED = DifferenceStatus' "UPDATED"

{-# COMPLETE
  DifferenceStatus_DELETED,
  DifferenceStatus_NEW,
  DifferenceStatus_UPDATED,
  DifferenceStatus'
  #-}
