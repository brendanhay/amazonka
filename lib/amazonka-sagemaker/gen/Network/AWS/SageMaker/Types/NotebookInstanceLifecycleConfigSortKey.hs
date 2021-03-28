{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.NotebookInstanceLifecycleConfigSortKey
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.NotebookInstanceLifecycleConfigSortKey
  ( NotebookInstanceLifecycleConfigSortKey
    ( NotebookInstanceLifecycleConfigSortKey'
    , NotebookInstanceLifecycleConfigSortKeyName
    , NotebookInstanceLifecycleConfigSortKeyCreationTime
    , NotebookInstanceLifecycleConfigSortKeyLastModifiedTime
    , fromNotebookInstanceLifecycleConfigSortKey
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype NotebookInstanceLifecycleConfigSortKey = NotebookInstanceLifecycleConfigSortKey'{fromNotebookInstanceLifecycleConfigSortKey
                                                                                         ::
                                                                                         Core.Text}
                                                   deriving stock (Core.Eq, Core.Ord, Core.Read,
                                                                   Core.Show, Core.Generic)
                                                   deriving newtype (Core.IsString, Core.Hashable,
                                                                     Core.NFData, Core.ToJSONKey,
                                                                     Core.FromJSONKey, Core.ToJSON,
                                                                     Core.FromJSON, Core.ToXML,
                                                                     Core.FromXML, Core.ToText,
                                                                     Core.FromText,
                                                                     Core.ToByteString,
                                                                     Core.ToQuery, Core.ToHeader)

pattern NotebookInstanceLifecycleConfigSortKeyName :: NotebookInstanceLifecycleConfigSortKey
pattern NotebookInstanceLifecycleConfigSortKeyName = NotebookInstanceLifecycleConfigSortKey' "Name"

pattern NotebookInstanceLifecycleConfigSortKeyCreationTime :: NotebookInstanceLifecycleConfigSortKey
pattern NotebookInstanceLifecycleConfigSortKeyCreationTime = NotebookInstanceLifecycleConfigSortKey' "CreationTime"

pattern NotebookInstanceLifecycleConfigSortKeyLastModifiedTime :: NotebookInstanceLifecycleConfigSortKey
pattern NotebookInstanceLifecycleConfigSortKeyLastModifiedTime = NotebookInstanceLifecycleConfigSortKey' "LastModifiedTime"

{-# COMPLETE 
  NotebookInstanceLifecycleConfigSortKeyName,

  NotebookInstanceLifecycleConfigSortKeyCreationTime,

  NotebookInstanceLifecycleConfigSortKeyLastModifiedTime,
  NotebookInstanceLifecycleConfigSortKey'
  #-}
