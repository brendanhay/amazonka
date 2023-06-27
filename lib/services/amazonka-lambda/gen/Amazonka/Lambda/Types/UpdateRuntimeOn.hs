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
-- Module      : Amazonka.Lambda.Types.UpdateRuntimeOn
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lambda.Types.UpdateRuntimeOn
  ( UpdateRuntimeOn
      ( ..,
        UpdateRuntimeOn_Auto,
        UpdateRuntimeOn_FunctionUpdate,
        UpdateRuntimeOn_Manual
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype UpdateRuntimeOn = UpdateRuntimeOn'
  { fromUpdateRuntimeOn ::
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

pattern UpdateRuntimeOn_Auto :: UpdateRuntimeOn
pattern UpdateRuntimeOn_Auto = UpdateRuntimeOn' "Auto"

pattern UpdateRuntimeOn_FunctionUpdate :: UpdateRuntimeOn
pattern UpdateRuntimeOn_FunctionUpdate = UpdateRuntimeOn' "FunctionUpdate"

pattern UpdateRuntimeOn_Manual :: UpdateRuntimeOn
pattern UpdateRuntimeOn_Manual = UpdateRuntimeOn' "Manual"

{-# COMPLETE
  UpdateRuntimeOn_Auto,
  UpdateRuntimeOn_FunctionUpdate,
  UpdateRuntimeOn_Manual,
  UpdateRuntimeOn'
  #-}
