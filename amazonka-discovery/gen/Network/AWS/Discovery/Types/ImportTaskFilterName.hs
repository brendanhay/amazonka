{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
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

import qualified Network.AWS.Prelude as Prelude

newtype ImportTaskFilterName = ImportTaskFilterName'
  { fromImportTaskFilterName ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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
