{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Discovery.Types.ImportTaskFilterName
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Discovery.Types.ImportTaskFilterName
  ( ImportTaskFilterName
      ( ImportTaskFilterName',
        ImportTaskId,
        Status,
        Name
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ImportTaskFilterName = ImportTaskFilterName' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern ImportTaskId :: ImportTaskFilterName
pattern ImportTaskId = ImportTaskFilterName' "IMPORT_TASK_ID"

pattern Status :: ImportTaskFilterName
pattern Status = ImportTaskFilterName' "STATUS"

pattern Name :: ImportTaskFilterName
pattern Name = ImportTaskFilterName' "NAME"

{-# COMPLETE
  ImportTaskId,
  Status,
  Name,
  ImportTaskFilterName'
  #-}
