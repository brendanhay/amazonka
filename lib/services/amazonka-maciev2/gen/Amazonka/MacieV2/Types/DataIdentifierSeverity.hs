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
-- Module      : Amazonka.MacieV2.Types.DataIdentifierSeverity
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.DataIdentifierSeverity
  ( DataIdentifierSeverity
      ( ..,
        DataIdentifierSeverity_HIGH,
        DataIdentifierSeverity_LOW,
        DataIdentifierSeverity_MEDIUM
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The severity of a finding, ranging from LOW, for least severe, to HIGH,
-- for most severe. Valid values are:
newtype DataIdentifierSeverity = DataIdentifierSeverity'
  { fromDataIdentifierSeverity ::
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

pattern DataIdentifierSeverity_HIGH :: DataIdentifierSeverity
pattern DataIdentifierSeverity_HIGH = DataIdentifierSeverity' "HIGH"

pattern DataIdentifierSeverity_LOW :: DataIdentifierSeverity
pattern DataIdentifierSeverity_LOW = DataIdentifierSeverity' "LOW"

pattern DataIdentifierSeverity_MEDIUM :: DataIdentifierSeverity
pattern DataIdentifierSeverity_MEDIUM = DataIdentifierSeverity' "MEDIUM"

{-# COMPLETE
  DataIdentifierSeverity_HIGH,
  DataIdentifierSeverity_LOW,
  DataIdentifierSeverity_MEDIUM,
  DataIdentifierSeverity'
  #-}
