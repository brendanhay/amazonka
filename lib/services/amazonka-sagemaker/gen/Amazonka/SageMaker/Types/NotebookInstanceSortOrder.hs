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
-- Module      : Amazonka.SageMaker.Types.NotebookInstanceSortOrder
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.NotebookInstanceSortOrder
  ( NotebookInstanceSortOrder
      ( ..,
        NotebookInstanceSortOrder_Ascending,
        NotebookInstanceSortOrder_Descending
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype NotebookInstanceSortOrder = NotebookInstanceSortOrder'
  { fromNotebookInstanceSortOrder ::
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

pattern NotebookInstanceSortOrder_Ascending :: NotebookInstanceSortOrder
pattern NotebookInstanceSortOrder_Ascending = NotebookInstanceSortOrder' "Ascending"

pattern NotebookInstanceSortOrder_Descending :: NotebookInstanceSortOrder
pattern NotebookInstanceSortOrder_Descending = NotebookInstanceSortOrder' "Descending"

{-# COMPLETE
  NotebookInstanceSortOrder_Ascending,
  NotebookInstanceSortOrder_Descending,
  NotebookInstanceSortOrder'
  #-}
