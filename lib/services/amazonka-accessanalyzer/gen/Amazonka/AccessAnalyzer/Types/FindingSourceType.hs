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
-- Module      : Amazonka.AccessAnalyzer.Types.FindingSourceType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AccessAnalyzer.Types.FindingSourceType
  ( FindingSourceType
      ( ..,
        FindingSourceType_BUCKET_ACL,
        FindingSourceType_POLICY,
        FindingSourceType_S3_ACCESS_POINT,
        FindingSourceType_S3_ACCESS_POINT_ACCOUNT
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype FindingSourceType = FindingSourceType'
  { fromFindingSourceType ::
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

pattern FindingSourceType_BUCKET_ACL :: FindingSourceType
pattern FindingSourceType_BUCKET_ACL = FindingSourceType' "BUCKET_ACL"

pattern FindingSourceType_POLICY :: FindingSourceType
pattern FindingSourceType_POLICY = FindingSourceType' "POLICY"

pattern FindingSourceType_S3_ACCESS_POINT :: FindingSourceType
pattern FindingSourceType_S3_ACCESS_POINT = FindingSourceType' "S3_ACCESS_POINT"

pattern FindingSourceType_S3_ACCESS_POINT_ACCOUNT :: FindingSourceType
pattern FindingSourceType_S3_ACCESS_POINT_ACCOUNT = FindingSourceType' "S3_ACCESS_POINT_ACCOUNT"

{-# COMPLETE
  FindingSourceType_BUCKET_ACL,
  FindingSourceType_POLICY,
  FindingSourceType_S3_ACCESS_POINT,
  FindingSourceType_S3_ACCESS_POINT_ACCOUNT,
  FindingSourceType'
  #-}
