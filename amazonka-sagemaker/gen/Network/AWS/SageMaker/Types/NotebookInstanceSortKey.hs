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
-- Module      : Network.AWS.SageMaker.Types.NotebookInstanceSortKey
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.NotebookInstanceSortKey
  ( NotebookInstanceSortKey
      ( ..,
        NotebookInstanceSortKey_CreationTime,
        NotebookInstanceSortKey_Name,
        NotebookInstanceSortKey_Status
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype NotebookInstanceSortKey = NotebookInstanceSortKey'
  { fromNotebookInstanceSortKey ::
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
