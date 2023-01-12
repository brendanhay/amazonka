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
-- Module      : Amazonka.SageMaker.Types.NotebookInstanceSortKey
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.NotebookInstanceSortKey
  ( NotebookInstanceSortKey
      ( ..,
        NotebookInstanceSortKey_CreationTime,
        NotebookInstanceSortKey_Name,
        NotebookInstanceSortKey_Status
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype NotebookInstanceSortKey = NotebookInstanceSortKey'
  { fromNotebookInstanceSortKey ::
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

pattern NotebookInstanceSortKey_CreationTime :: NotebookInstanceSortKey
pattern NotebookInstanceSortKey_CreationTime = NotebookInstanceSortKey' "CreationTime"

pattern NotebookInstanceSortKey_Name :: NotebookInstanceSortKey
pattern NotebookInstanceSortKey_Name = NotebookInstanceSortKey' "Name"

pattern NotebookInstanceSortKey_Status :: NotebookInstanceSortKey
pattern NotebookInstanceSortKey_Status = NotebookInstanceSortKey' "Status"

{-# COMPLETE
  NotebookInstanceSortKey_CreationTime,
  NotebookInstanceSortKey_Name,
  NotebookInstanceSortKey_Status,
  NotebookInstanceSortKey'
  #-}
