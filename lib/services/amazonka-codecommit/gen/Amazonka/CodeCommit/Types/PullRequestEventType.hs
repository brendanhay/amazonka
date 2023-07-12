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
-- Module      : Amazonka.CodeCommit.Types.PullRequestEventType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeCommit.Types.PullRequestEventType
  ( PullRequestEventType
      ( ..,
        PullRequestEventType_PULL_REQUEST_APPROVAL_RULE_CREATED,
        PullRequestEventType_PULL_REQUEST_APPROVAL_RULE_DELETED,
        PullRequestEventType_PULL_REQUEST_APPROVAL_RULE_OVERRIDDEN,
        PullRequestEventType_PULL_REQUEST_APPROVAL_RULE_UPDATED,
        PullRequestEventType_PULL_REQUEST_APPROVAL_STATE_CHANGED,
        PullRequestEventType_PULL_REQUEST_CREATED,
        PullRequestEventType_PULL_REQUEST_MERGE_STATE_CHANGED,
        PullRequestEventType_PULL_REQUEST_SOURCE_REFERENCE_UPDATED,
        PullRequestEventType_PULL_REQUEST_STATUS_CHANGED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype PullRequestEventType = PullRequestEventType'
  { fromPullRequestEventType ::
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

pattern PullRequestEventType_PULL_REQUEST_APPROVAL_RULE_CREATED :: PullRequestEventType
pattern PullRequestEventType_PULL_REQUEST_APPROVAL_RULE_CREATED = PullRequestEventType' "PULL_REQUEST_APPROVAL_RULE_CREATED"

pattern PullRequestEventType_PULL_REQUEST_APPROVAL_RULE_DELETED :: PullRequestEventType
pattern PullRequestEventType_PULL_REQUEST_APPROVAL_RULE_DELETED = PullRequestEventType' "PULL_REQUEST_APPROVAL_RULE_DELETED"

pattern PullRequestEventType_PULL_REQUEST_APPROVAL_RULE_OVERRIDDEN :: PullRequestEventType
pattern PullRequestEventType_PULL_REQUEST_APPROVAL_RULE_OVERRIDDEN = PullRequestEventType' "PULL_REQUEST_APPROVAL_RULE_OVERRIDDEN"

pattern PullRequestEventType_PULL_REQUEST_APPROVAL_RULE_UPDATED :: PullRequestEventType
pattern PullRequestEventType_PULL_REQUEST_APPROVAL_RULE_UPDATED = PullRequestEventType' "PULL_REQUEST_APPROVAL_RULE_UPDATED"

pattern PullRequestEventType_PULL_REQUEST_APPROVAL_STATE_CHANGED :: PullRequestEventType
pattern PullRequestEventType_PULL_REQUEST_APPROVAL_STATE_CHANGED = PullRequestEventType' "PULL_REQUEST_APPROVAL_STATE_CHANGED"

pattern PullRequestEventType_PULL_REQUEST_CREATED :: PullRequestEventType
pattern PullRequestEventType_PULL_REQUEST_CREATED = PullRequestEventType' "PULL_REQUEST_CREATED"

pattern PullRequestEventType_PULL_REQUEST_MERGE_STATE_CHANGED :: PullRequestEventType
pattern PullRequestEventType_PULL_REQUEST_MERGE_STATE_CHANGED = PullRequestEventType' "PULL_REQUEST_MERGE_STATE_CHANGED"

pattern PullRequestEventType_PULL_REQUEST_SOURCE_REFERENCE_UPDATED :: PullRequestEventType
pattern PullRequestEventType_PULL_REQUEST_SOURCE_REFERENCE_UPDATED = PullRequestEventType' "PULL_REQUEST_SOURCE_REFERENCE_UPDATED"

pattern PullRequestEventType_PULL_REQUEST_STATUS_CHANGED :: PullRequestEventType
pattern PullRequestEventType_PULL_REQUEST_STATUS_CHANGED = PullRequestEventType' "PULL_REQUEST_STATUS_CHANGED"

{-# COMPLETE
  PullRequestEventType_PULL_REQUEST_APPROVAL_RULE_CREATED,
  PullRequestEventType_PULL_REQUEST_APPROVAL_RULE_DELETED,
  PullRequestEventType_PULL_REQUEST_APPROVAL_RULE_OVERRIDDEN,
  PullRequestEventType_PULL_REQUEST_APPROVAL_RULE_UPDATED,
  PullRequestEventType_PULL_REQUEST_APPROVAL_STATE_CHANGED,
  PullRequestEventType_PULL_REQUEST_CREATED,
  PullRequestEventType_PULL_REQUEST_MERGE_STATE_CHANGED,
  PullRequestEventType_PULL_REQUEST_SOURCE_REFERENCE_UPDATED,
  PullRequestEventType_PULL_REQUEST_STATUS_CHANGED,
  PullRequestEventType'
  #-}
