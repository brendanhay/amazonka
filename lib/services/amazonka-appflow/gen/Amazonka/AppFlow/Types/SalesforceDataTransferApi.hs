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
-- Module      : Amazonka.AppFlow.Types.SalesforceDataTransferApi
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.SalesforceDataTransferApi
  ( SalesforceDataTransferApi
      ( ..,
        SalesforceDataTransferApi_AUTOMATIC,
        SalesforceDataTransferApi_BULKV2,
        SalesforceDataTransferApi_REST_SYNC
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype SalesforceDataTransferApi = SalesforceDataTransferApi'
  { fromSalesforceDataTransferApi ::
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

pattern SalesforceDataTransferApi_AUTOMATIC :: SalesforceDataTransferApi
pattern SalesforceDataTransferApi_AUTOMATIC = SalesforceDataTransferApi' "AUTOMATIC"

pattern SalesforceDataTransferApi_BULKV2 :: SalesforceDataTransferApi
pattern SalesforceDataTransferApi_BULKV2 = SalesforceDataTransferApi' "BULKV2"

pattern SalesforceDataTransferApi_REST_SYNC :: SalesforceDataTransferApi
pattern SalesforceDataTransferApi_REST_SYNC = SalesforceDataTransferApi' "REST_SYNC"

{-# COMPLETE
  SalesforceDataTransferApi_AUTOMATIC,
  SalesforceDataTransferApi_BULKV2,
  SalesforceDataTransferApi_REST_SYNC,
  SalesforceDataTransferApi'
  #-}
