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
-- Module      : Amazonka.Batch.Types.ArrayJobDependency
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Batch.Types.ArrayJobDependency
  ( ArrayJobDependency
      ( ..,
        ArrayJobDependency_N_TO_N,
        ArrayJobDependency_SEQUENTIAL
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ArrayJobDependency = ArrayJobDependency'
  { fromArrayJobDependency ::
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

pattern ArrayJobDependency_N_TO_N :: ArrayJobDependency
pattern ArrayJobDependency_N_TO_N = ArrayJobDependency' "N_TO_N"

pattern ArrayJobDependency_SEQUENTIAL :: ArrayJobDependency
pattern ArrayJobDependency_SEQUENTIAL = ArrayJobDependency' "SEQUENTIAL"

{-# COMPLETE
  ArrayJobDependency_N_TO_N,
  ArrayJobDependency_SEQUENTIAL,
  ArrayJobDependency'
  #-}
