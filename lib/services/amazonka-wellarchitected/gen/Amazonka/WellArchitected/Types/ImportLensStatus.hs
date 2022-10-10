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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
import qualified Amazonka.Prelude as Prelude

newtype ImportLensStatus = ImportLensStatus'
  { fromImportLensStatus ::
      Core.Text
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
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
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
