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
-- Module      : Amazonka.Discovery.Types.ImportTaskFilterName
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Discovery.Types.ImportTaskFilterName
  ( ImportTaskFilterName
      ( ..,
        ImportTaskFilterName_IMPORT_TASK_ID,
        ImportTaskFilterName_NAME,
        ImportTaskFilterName_STATUS
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype ImportTaskFilterName = ImportTaskFilterName'
  { fromImportTaskFilterName ::
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

pattern ImportTaskFilterName_IMPORT_TASK_ID :: ImportTaskFilterName
pattern ImportTaskFilterName_IMPORT_TASK_ID = ImportTaskFilterName' "IMPORT_TASK_ID"

pattern ImportTaskFilterName_NAME :: ImportTaskFilterName
pattern ImportTaskFilterName_NAME = ImportTaskFilterName' "NAME"

pattern ImportTaskFilterName_STATUS :: ImportTaskFilterName
pattern ImportTaskFilterName_STATUS = ImportTaskFilterName' "STATUS"

{-# COMPLETE
  ImportTaskFilterName_IMPORT_TASK_ID,
  ImportTaskFilterName_NAME,
  ImportTaskFilterName_STATUS,
  ImportTaskFilterName'
  #-}
