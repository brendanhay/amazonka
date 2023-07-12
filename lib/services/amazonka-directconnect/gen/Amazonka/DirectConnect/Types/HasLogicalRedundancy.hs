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
-- Module      : Amazonka.DirectConnect.Types.HasLogicalRedundancy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DirectConnect.Types.HasLogicalRedundancy
  ( HasLogicalRedundancy
      ( ..,
        HasLogicalRedundancy_No,
        HasLogicalRedundancy_Unknown,
        HasLogicalRedundancy_Yes
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype HasLogicalRedundancy = HasLogicalRedundancy'
  { fromHasLogicalRedundancy ::
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

pattern HasLogicalRedundancy_No :: HasLogicalRedundancy
pattern HasLogicalRedundancy_No = HasLogicalRedundancy' "no"

pattern HasLogicalRedundancy_Unknown :: HasLogicalRedundancy
pattern HasLogicalRedundancy_Unknown = HasLogicalRedundancy' "unknown"

pattern HasLogicalRedundancy_Yes :: HasLogicalRedundancy
pattern HasLogicalRedundancy_Yes = HasLogicalRedundancy' "yes"

{-# COMPLETE
  HasLogicalRedundancy_No,
  HasLogicalRedundancy_Unknown,
  HasLogicalRedundancy_Yes,
  HasLogicalRedundancy'
  #-}
