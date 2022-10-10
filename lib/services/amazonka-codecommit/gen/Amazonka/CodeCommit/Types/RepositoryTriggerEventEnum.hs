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
-- Module      : Amazonka.CodeCommit.Types.RepositoryTriggerEventEnum
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeCommit.Types.RepositoryTriggerEventEnum
  ( RepositoryTriggerEventEnum
      ( ..,
        RepositoryTriggerEventEnum_All,
        RepositoryTriggerEventEnum_CreateReference,
        RepositoryTriggerEventEnum_DeleteReference,
        RepositoryTriggerEventEnum_UpdateReference
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype RepositoryTriggerEventEnum = RepositoryTriggerEventEnum'
  { fromRepositoryTriggerEventEnum ::
      Core.Text
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
