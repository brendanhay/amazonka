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
-- Module      : Amazonka.QuickSight.Types.MaximumMinimumComputationType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.MaximumMinimumComputationType
  ( MaximumMinimumComputationType
      ( ..,
        MaximumMinimumComputationType_MAXIMUM,
        MaximumMinimumComputationType_MINIMUM
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype MaximumMinimumComputationType = MaximumMinimumComputationType'
  { fromMaximumMinimumComputationType ::
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

pattern MaximumMinimumComputationType_MAXIMUM :: MaximumMinimumComputationType
pattern MaximumMinimumComputationType_MAXIMUM = MaximumMinimumComputationType' "MAXIMUM"

pattern MaximumMinimumComputationType_MINIMUM :: MaximumMinimumComputationType
pattern MaximumMinimumComputationType_MINIMUM = MaximumMinimumComputationType' "MINIMUM"

{-# COMPLETE
  MaximumMinimumComputationType_MAXIMUM,
  MaximumMinimumComputationType_MINIMUM,
  MaximumMinimumComputationType'
  #-}
