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
-- Module      : Network.AWS.ECS.Types.TaskDefinitionFamilyStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.TaskDefinitionFamilyStatus
  ( TaskDefinitionFamilyStatus
      ( ..,
        TaskDefinitionFamilyStatus_ACTIVE,
        TaskDefinitionFamilyStatus_ALL,
        TaskDefinitionFamilyStatus_INACTIVE
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype TaskDefinitionFamilyStatus = TaskDefinitionFamilyStatus'
  { fromTaskDefinitionFamilyStatus ::
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

pattern TaskDefinitionFamilyStatus_ACTIVE :: TaskDefinitionFamilyStatus
pattern TaskDefinitionFamilyStatus_ACTIVE = TaskDefinitionFamilyStatus' "ACTIVE"

pattern TaskDefinitionFamilyStatus_ALL :: TaskDefinitionFamilyStatus
pattern TaskDefinitionFamilyStatus_ALL = TaskDefinitionFamilyStatus' "ALL"

pattern TaskDefinitionFamilyStatus_INACTIVE :: TaskDefinitionFamilyStatus
pattern TaskDefinitionFamilyStatus_INACTIVE = TaskDefinitionFamilyStatus' "INACTIVE"

{-# COMPLETE
  TaskDefinitionFamilyStatus_ACTIVE,
  TaskDefinitionFamilyStatus_ALL,
  TaskDefinitionFamilyStatus_INACTIVE,
  TaskDefinitionFamilyStatus'
  #-}
