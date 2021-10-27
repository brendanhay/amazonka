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
-- Module      : Network.AWS.Wisdom.Types.KnowledgeBaseStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Wisdom.Types.KnowledgeBaseStatus
  ( KnowledgeBaseStatus
      ( ..,
        KnowledgeBaseStatus_ACTIVE,
        KnowledgeBaseStatus_CREATE_FAILED,
        KnowledgeBaseStatus_CREATE_IN_PROGRESS,
        KnowledgeBaseStatus_DELETED,
        KnowledgeBaseStatus_DELETE_FAILED,
        KnowledgeBaseStatus_DELETE_IN_PROGRESS
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype KnowledgeBaseStatus = KnowledgeBaseStatus'
  { fromKnowledgeBaseStatus ::
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

pattern KnowledgeBaseStatus_ACTIVE :: KnowledgeBaseStatus
pattern KnowledgeBaseStatus_ACTIVE = KnowledgeBaseStatus' "ACTIVE"

pattern KnowledgeBaseStatus_CREATE_FAILED :: KnowledgeBaseStatus
pattern KnowledgeBaseStatus_CREATE_FAILED = KnowledgeBaseStatus' "CREATE_FAILED"

pattern KnowledgeBaseStatus_CREATE_IN_PROGRESS :: KnowledgeBaseStatus
pattern KnowledgeBaseStatus_CREATE_IN_PROGRESS = KnowledgeBaseStatus' "CREATE_IN_PROGRESS"

pattern KnowledgeBaseStatus_DELETED :: KnowledgeBaseStatus
pattern KnowledgeBaseStatus_DELETED = KnowledgeBaseStatus' "DELETED"

pattern KnowledgeBaseStatus_DELETE_FAILED :: KnowledgeBaseStatus
pattern KnowledgeBaseStatus_DELETE_FAILED = KnowledgeBaseStatus' "DELETE_FAILED"

pattern KnowledgeBaseStatus_DELETE_IN_PROGRESS :: KnowledgeBaseStatus
pattern KnowledgeBaseStatus_DELETE_IN_PROGRESS = KnowledgeBaseStatus' "DELETE_IN_PROGRESS"

{-# COMPLETE
  KnowledgeBaseStatus_ACTIVE,
  KnowledgeBaseStatus_CREATE_FAILED,
  KnowledgeBaseStatus_CREATE_IN_PROGRESS,
  KnowledgeBaseStatus_DELETED,
  KnowledgeBaseStatus_DELETE_FAILED,
  KnowledgeBaseStatus_DELETE_IN_PROGRESS,
  KnowledgeBaseStatus'
  #-}
