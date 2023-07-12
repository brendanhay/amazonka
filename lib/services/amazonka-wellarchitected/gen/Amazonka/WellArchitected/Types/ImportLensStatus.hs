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
-- Module      : Amazonka.WellArchitected.Types.ImportLensStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WellArchitected.Types.ImportLensStatus
  ( ImportLensStatus
      ( ..,
        ImportLensStatus_COMPLETE,
        ImportLensStatus_ERROR,
        ImportLensStatus_IN_PROGRESS
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ImportLensStatus = ImportLensStatus'
  { fromImportLensStatus ::
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

pattern ImportLensStatus_COMPLETE :: ImportLensStatus
pattern ImportLensStatus_COMPLETE = ImportLensStatus' "COMPLETE"

pattern ImportLensStatus_ERROR :: ImportLensStatus
pattern ImportLensStatus_ERROR = ImportLensStatus' "ERROR"

pattern ImportLensStatus_IN_PROGRESS :: ImportLensStatus
pattern ImportLensStatus_IN_PROGRESS = ImportLensStatus' "IN_PROGRESS"

{-# COMPLETE
  ImportLensStatus_COMPLETE,
  ImportLensStatus_ERROR,
  ImportLensStatus_IN_PROGRESS,
  ImportLensStatus'
  #-}
