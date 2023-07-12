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
-- Module      : Amazonka.Connect.Types.TaskTemplateFieldType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.TaskTemplateFieldType
  ( TaskTemplateFieldType
      ( ..,
        TaskTemplateFieldType_BOOLEAN,
        TaskTemplateFieldType_DATE_TIME,
        TaskTemplateFieldType_DESCRIPTION,
        TaskTemplateFieldType_EMAIL,
        TaskTemplateFieldType_NAME,
        TaskTemplateFieldType_NUMBER,
        TaskTemplateFieldType_QUICK_CONNECT,
        TaskTemplateFieldType_SCHEDULED_TIME,
        TaskTemplateFieldType_SINGLE_SELECT,
        TaskTemplateFieldType_TEXT,
        TaskTemplateFieldType_TEXT_AREA,
        TaskTemplateFieldType_URL
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype TaskTemplateFieldType = TaskTemplateFieldType'
  { fromTaskTemplateFieldType ::
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

pattern TaskTemplateFieldType_BOOLEAN :: TaskTemplateFieldType
pattern TaskTemplateFieldType_BOOLEAN = TaskTemplateFieldType' "BOOLEAN"

pattern TaskTemplateFieldType_DATE_TIME :: TaskTemplateFieldType
pattern TaskTemplateFieldType_DATE_TIME = TaskTemplateFieldType' "DATE_TIME"

pattern TaskTemplateFieldType_DESCRIPTION :: TaskTemplateFieldType
pattern TaskTemplateFieldType_DESCRIPTION = TaskTemplateFieldType' "DESCRIPTION"

pattern TaskTemplateFieldType_EMAIL :: TaskTemplateFieldType
pattern TaskTemplateFieldType_EMAIL = TaskTemplateFieldType' "EMAIL"

pattern TaskTemplateFieldType_NAME :: TaskTemplateFieldType
pattern TaskTemplateFieldType_NAME = TaskTemplateFieldType' "NAME"

pattern TaskTemplateFieldType_NUMBER :: TaskTemplateFieldType
pattern TaskTemplateFieldType_NUMBER = TaskTemplateFieldType' "NUMBER"

pattern TaskTemplateFieldType_QUICK_CONNECT :: TaskTemplateFieldType
pattern TaskTemplateFieldType_QUICK_CONNECT = TaskTemplateFieldType' "QUICK_CONNECT"

pattern TaskTemplateFieldType_SCHEDULED_TIME :: TaskTemplateFieldType
pattern TaskTemplateFieldType_SCHEDULED_TIME = TaskTemplateFieldType' "SCHEDULED_TIME"

pattern TaskTemplateFieldType_SINGLE_SELECT :: TaskTemplateFieldType
pattern TaskTemplateFieldType_SINGLE_SELECT = TaskTemplateFieldType' "SINGLE_SELECT"

pattern TaskTemplateFieldType_TEXT :: TaskTemplateFieldType
pattern TaskTemplateFieldType_TEXT = TaskTemplateFieldType' "TEXT"

pattern TaskTemplateFieldType_TEXT_AREA :: TaskTemplateFieldType
pattern TaskTemplateFieldType_TEXT_AREA = TaskTemplateFieldType' "TEXT_AREA"

pattern TaskTemplateFieldType_URL :: TaskTemplateFieldType
pattern TaskTemplateFieldType_URL = TaskTemplateFieldType' "URL"

{-# COMPLETE
  TaskTemplateFieldType_BOOLEAN,
  TaskTemplateFieldType_DATE_TIME,
  TaskTemplateFieldType_DESCRIPTION,
  TaskTemplateFieldType_EMAIL,
  TaskTemplateFieldType_NAME,
  TaskTemplateFieldType_NUMBER,
  TaskTemplateFieldType_QUICK_CONNECT,
  TaskTemplateFieldType_SCHEDULED_TIME,
  TaskTemplateFieldType_SINGLE_SELECT,
  TaskTemplateFieldType_TEXT,
  TaskTemplateFieldType_TEXT_AREA,
  TaskTemplateFieldType_URL,
  TaskTemplateFieldType'
  #-}
