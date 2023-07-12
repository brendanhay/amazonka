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
-- Module      : Amazonka.Inspector2.Types.GroupKey
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector2.Types.GroupKey
  ( GroupKey
      ( ..,
        GroupKey_ACCOUNT_ID,
        GroupKey_ECR_REPOSITORY_NAME,
        GroupKey_RESOURCE_TYPE,
        GroupKey_SCAN_STATUS_CODE,
        GroupKey_SCAN_STATUS_REASON
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype GroupKey = GroupKey'
  { fromGroupKey ::
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

pattern GroupKey_ACCOUNT_ID :: GroupKey
pattern GroupKey_ACCOUNT_ID = GroupKey' "ACCOUNT_ID"

pattern GroupKey_ECR_REPOSITORY_NAME :: GroupKey
pattern GroupKey_ECR_REPOSITORY_NAME = GroupKey' "ECR_REPOSITORY_NAME"

pattern GroupKey_RESOURCE_TYPE :: GroupKey
pattern GroupKey_RESOURCE_TYPE = GroupKey' "RESOURCE_TYPE"

pattern GroupKey_SCAN_STATUS_CODE :: GroupKey
pattern GroupKey_SCAN_STATUS_CODE = GroupKey' "SCAN_STATUS_CODE"

pattern GroupKey_SCAN_STATUS_REASON :: GroupKey
pattern GroupKey_SCAN_STATUS_REASON = GroupKey' "SCAN_STATUS_REASON"

{-# COMPLETE
  GroupKey_ACCOUNT_ID,
  GroupKey_ECR_REPOSITORY_NAME,
  GroupKey_RESOURCE_TYPE,
  GroupKey_SCAN_STATUS_CODE,
  GroupKey_SCAN_STATUS_REASON,
  GroupKey'
  #-}
