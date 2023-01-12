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
-- Module      : Amazonka.MacieV2.Types.IsDefinedInJob
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.IsDefinedInJob
  ( IsDefinedInJob
      ( ..,
        IsDefinedInJob_FALSE,
        IsDefinedInJob_TRUE,
        IsDefinedInJob_UNKNOWN
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype IsDefinedInJob = IsDefinedInJob'
  { fromIsDefinedInJob ::
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

pattern IsDefinedInJob_FALSE :: IsDefinedInJob
pattern IsDefinedInJob_FALSE = IsDefinedInJob' "FALSE"

pattern IsDefinedInJob_TRUE :: IsDefinedInJob
pattern IsDefinedInJob_TRUE = IsDefinedInJob' "TRUE"

pattern IsDefinedInJob_UNKNOWN :: IsDefinedInJob
pattern IsDefinedInJob_UNKNOWN = IsDefinedInJob' "UNKNOWN"

{-# COMPLETE
  IsDefinedInJob_FALSE,
  IsDefinedInJob_TRUE,
  IsDefinedInJob_UNKNOWN,
  IsDefinedInJob'
  #-}
