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
-- Module      : Network.AWS.DMS.Types.SafeguardPolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.SafeguardPolicy
  ( SafeguardPolicy
      ( ..,
        SafeguardPolicy_Exclusive_automatic_truncation,
        SafeguardPolicy_Rely_on_sql_server_replication_agent,
        SafeguardPolicy_Shared_automatic_truncation
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype SafeguardPolicy = SafeguardPolicy'
  { fromSafeguardPolicy ::
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

pattern SafeguardPolicy_Exclusive_automatic_truncation :: SafeguardPolicy
pattern SafeguardPolicy_Exclusive_automatic_truncation = SafeguardPolicy' "exclusive-automatic-truncation"

pattern SafeguardPolicy_Rely_on_sql_server_replication_agent :: SafeguardPolicy
pattern SafeguardPolicy_Rely_on_sql_server_replication_agent = SafeguardPolicy' "rely-on-sql-server-replication-agent"

pattern SafeguardPolicy_Shared_automatic_truncation :: SafeguardPolicy
pattern SafeguardPolicy_Shared_automatic_truncation = SafeguardPolicy' "shared-automatic-truncation"

{-# COMPLETE
  SafeguardPolicy_Exclusive_automatic_truncation,
  SafeguardPolicy_Rely_on_sql_server_replication_agent,
  SafeguardPolicy_Shared_automatic_truncation,
  SafeguardPolicy'
  #-}
