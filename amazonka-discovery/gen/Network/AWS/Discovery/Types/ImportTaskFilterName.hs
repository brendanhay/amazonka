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
-- Module      : Network.AWS.Discovery.Types.ImportTaskFilterName
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Discovery.Types.ImportTaskFilterName
  ( ImportTaskFilterName
      ( ..,
        ImportTaskFilterName_IMPORT_TASK_ID,
        ImportTaskFilterName_NAME,
        ImportTaskFilterName_STATUS
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype ImportTaskFilterName = ImportTaskFilterName'
  { fromImportTaskFilterName ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
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
