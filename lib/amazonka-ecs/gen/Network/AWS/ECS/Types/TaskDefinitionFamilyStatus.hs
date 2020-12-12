{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.TaskDefinitionFamilyStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.TaskDefinitionFamilyStatus
  ( TaskDefinitionFamilyStatus
      ( TaskDefinitionFamilyStatus',
        TDFSActive,
        TDFSAll,
        TDFSInactive
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype TaskDefinitionFamilyStatus = TaskDefinitionFamilyStatus' Lude.Text
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

pattern TDFSActive :: TaskDefinitionFamilyStatus
pattern TDFSActive = TaskDefinitionFamilyStatus' "ACTIVE"

pattern TDFSAll :: TaskDefinitionFamilyStatus
pattern TDFSAll = TaskDefinitionFamilyStatus' "ALL"

pattern TDFSInactive :: TaskDefinitionFamilyStatus
pattern TDFSInactive = TaskDefinitionFamilyStatus' "INACTIVE"

{-# COMPLETE
  TDFSActive,
  TDFSAll,
  TDFSInactive,
  TaskDefinitionFamilyStatus'
  #-}
