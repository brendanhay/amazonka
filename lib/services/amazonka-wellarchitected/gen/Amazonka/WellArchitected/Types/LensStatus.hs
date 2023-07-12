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
-- Module      : Amazonka.WellArchitected.Types.LensStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WellArchitected.Types.LensStatus
  ( LensStatus
      ( ..,
        LensStatus_CURRENT,
        LensStatus_DELETED,
        LensStatus_DEPRECATED,
        LensStatus_NOT_CURRENT,
        LensStatus_UNSHARED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype LensStatus = LensStatus'
  { fromLensStatus ::
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

pattern LensStatus_CURRENT :: LensStatus
pattern LensStatus_CURRENT = LensStatus' "CURRENT"

pattern LensStatus_DELETED :: LensStatus
pattern LensStatus_DELETED = LensStatus' "DELETED"

pattern LensStatus_DEPRECATED :: LensStatus
pattern LensStatus_DEPRECATED = LensStatus' "DEPRECATED"

pattern LensStatus_NOT_CURRENT :: LensStatus
pattern LensStatus_NOT_CURRENT = LensStatus' "NOT_CURRENT"

pattern LensStatus_UNSHARED :: LensStatus
pattern LensStatus_UNSHARED = LensStatus' "UNSHARED"

{-# COMPLETE
  LensStatus_CURRENT,
  LensStatus_DELETED,
  LensStatus_DEPRECATED,
  LensStatus_NOT_CURRENT,
  LensStatus_UNSHARED,
  LensStatus'
  #-}
