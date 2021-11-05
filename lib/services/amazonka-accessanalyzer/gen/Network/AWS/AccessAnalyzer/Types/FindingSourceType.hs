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
-- Module      : Network.AWS.AccessAnalyzer.Types.FindingSourceType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AccessAnalyzer.Types.FindingSourceType
  ( FindingSourceType
      ( ..,
        FindingSourceType_BUCKET_ACL,
        FindingSourceType_POLICY,
        FindingSourceType_S3_ACCESS_POINT
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype FindingSourceType = FindingSourceType'
  { fromFindingSourceType ::
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

pattern FindingSourceType_BUCKET_ACL :: FindingSourceType
pattern FindingSourceType_BUCKET_ACL = FindingSourceType' "BUCKET_ACL"

pattern FindingSourceType_POLICY :: FindingSourceType
pattern FindingSourceType_POLICY = FindingSourceType' "POLICY"

pattern FindingSourceType_S3_ACCESS_POINT :: FindingSourceType
pattern FindingSourceType_S3_ACCESS_POINT = FindingSourceType' "S3_ACCESS_POINT"

{-# COMPLETE
  FindingSourceType_BUCKET_ACL,
  FindingSourceType_POLICY,
  FindingSourceType_S3_ACCESS_POINT,
  FindingSourceType'
  #-}
