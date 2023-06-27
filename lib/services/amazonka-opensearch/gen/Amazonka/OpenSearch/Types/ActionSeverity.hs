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
-- Module      : Amazonka.OpenSearch.Types.ActionSeverity
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpenSearch.Types.ActionSeverity
  ( ActionSeverity
      ( ..,
        ActionSeverity_HIGH,
        ActionSeverity_LOW,
        ActionSeverity_MEDIUM
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ActionSeverity = ActionSeverity'
  { fromActionSeverity ::
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

pattern ActionSeverity_HIGH :: ActionSeverity
pattern ActionSeverity_HIGH = ActionSeverity' "HIGH"

pattern ActionSeverity_LOW :: ActionSeverity
pattern ActionSeverity_LOW = ActionSeverity' "LOW"

pattern ActionSeverity_MEDIUM :: ActionSeverity
pattern ActionSeverity_MEDIUM = ActionSeverity' "MEDIUM"

{-# COMPLETE
  ActionSeverity_HIGH,
  ActionSeverity_LOW,
  ActionSeverity_MEDIUM,
  ActionSeverity'
  #-}
