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
-- Module      : Amazonka.DMS.Types.SafeguardPolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DMS.Types.SafeguardPolicy
  ( SafeguardPolicy
      ( ..,
        SafeguardPolicy_Exclusive_automatic_truncation,
        SafeguardPolicy_Rely_on_sql_server_replication_agent,
        SafeguardPolicy_Shared_automatic_truncation
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype SafeguardPolicy = SafeguardPolicy'
  { fromSafeguardPolicy ::
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
