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
-- Module      : Amazonka.OpenSearch.Types.ConnectionMode
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpenSearch.Types.ConnectionMode
  ( ConnectionMode
      ( ..,
        ConnectionMode_DIRECT,
        ConnectionMode_VPC_ENDPOINT
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The connection mode for the cross-cluster connection.
--
-- -   __DIRECT__ - Used for cross-cluster search or cross-cluster
--     replication.
--
-- -   __VPC_ENDPOINT__ - Used for remote reindex between Amazon OpenSearch
--     Service VPC domains.
newtype ConnectionMode = ConnectionMode'
  { fromConnectionMode ::
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

pattern ConnectionMode_DIRECT :: ConnectionMode
pattern ConnectionMode_DIRECT = ConnectionMode' "DIRECT"

pattern ConnectionMode_VPC_ENDPOINT :: ConnectionMode
pattern ConnectionMode_VPC_ENDPOINT = ConnectionMode' "VPC_ENDPOINT"

{-# COMPLETE
  ConnectionMode_DIRECT,
  ConnectionMode_VPC_ENDPOINT,
  ConnectionMode'
  #-}
