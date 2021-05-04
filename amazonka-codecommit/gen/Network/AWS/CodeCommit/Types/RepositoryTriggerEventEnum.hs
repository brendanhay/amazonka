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
-- Module      : Network.AWS.CodeCommit.Types.RepositoryTriggerEventEnum
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.RepositoryTriggerEventEnum
  ( RepositoryTriggerEventEnum
      ( ..,
        RepositoryTriggerEventEnum_All,
        RepositoryTriggerEventEnum_CreateReference,
        RepositoryTriggerEventEnum_DeleteReference,
        RepositoryTriggerEventEnum_UpdateReference
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype RepositoryTriggerEventEnum = RepositoryTriggerEventEnum'
  { fromRepositoryTriggerEventEnum ::
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

pattern RepositoryTriggerEventEnum_All :: RepositoryTriggerEventEnum
pattern RepositoryTriggerEventEnum_All = RepositoryTriggerEventEnum' "all"

pattern RepositoryTriggerEventEnum_CreateReference :: RepositoryTriggerEventEnum
pattern RepositoryTriggerEventEnum_CreateReference = RepositoryTriggerEventEnum' "createReference"

pattern RepositoryTriggerEventEnum_DeleteReference :: RepositoryTriggerEventEnum
pattern RepositoryTriggerEventEnum_DeleteReference = RepositoryTriggerEventEnum' "deleteReference"

pattern RepositoryTriggerEventEnum_UpdateReference :: RepositoryTriggerEventEnum
pattern RepositoryTriggerEventEnum_UpdateReference = RepositoryTriggerEventEnum' "updateReference"

{-# COMPLETE
  RepositoryTriggerEventEnum_All,
  RepositoryTriggerEventEnum_CreateReference,
  RepositoryTriggerEventEnum_DeleteReference,
  RepositoryTriggerEventEnum_UpdateReference,
  RepositoryTriggerEventEnum'
  #-}
