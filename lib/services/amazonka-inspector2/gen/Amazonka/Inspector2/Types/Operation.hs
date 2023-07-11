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
-- Module      : Amazonka.Inspector2.Types.Operation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector2.Types.Operation
  ( Operation
      ( ..,
        Operation_DISABLE_REPOSITORY,
        Operation_DISABLE_SCANNING,
        Operation_ENABLE_REPOSITORY,
        Operation_ENABLE_SCANNING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype Operation = Operation'
  { fromOperation ::
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

pattern Operation_DISABLE_REPOSITORY :: Operation
pattern Operation_DISABLE_REPOSITORY = Operation' "DISABLE_REPOSITORY"

pattern Operation_DISABLE_SCANNING :: Operation
pattern Operation_DISABLE_SCANNING = Operation' "DISABLE_SCANNING"

pattern Operation_ENABLE_REPOSITORY :: Operation
pattern Operation_ENABLE_REPOSITORY = Operation' "ENABLE_REPOSITORY"

pattern Operation_ENABLE_SCANNING :: Operation
pattern Operation_ENABLE_SCANNING = Operation' "ENABLE_SCANNING"

{-# COMPLETE
  Operation_DISABLE_REPOSITORY,
  Operation_DISABLE_SCANNING,
  Operation_ENABLE_REPOSITORY,
  Operation_ENABLE_SCANNING,
  Operation'
  #-}
