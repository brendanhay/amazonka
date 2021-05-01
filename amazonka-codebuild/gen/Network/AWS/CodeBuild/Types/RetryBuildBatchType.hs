{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.RetryBuildBatchType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.RetryBuildBatchType
  ( RetryBuildBatchType
      ( ..,
        RetryBuildBatchType_RETRY_ALL_BUILDS,
        RetryBuildBatchType_RETRY_FAILED_BUILDS
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype RetryBuildBatchType = RetryBuildBatchType'
  { fromRetryBuildBatchType ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
    )

pattern RetryBuildBatchType_RETRY_ALL_BUILDS :: RetryBuildBatchType
pattern RetryBuildBatchType_RETRY_ALL_BUILDS = RetryBuildBatchType' "RETRY_ALL_BUILDS"

pattern RetryBuildBatchType_RETRY_FAILED_BUILDS :: RetryBuildBatchType
pattern RetryBuildBatchType_RETRY_FAILED_BUILDS = RetryBuildBatchType' "RETRY_FAILED_BUILDS"

{-# COMPLETE
  RetryBuildBatchType_RETRY_ALL_BUILDS,
  RetryBuildBatchType_RETRY_FAILED_BUILDS,
  RetryBuildBatchType'
  #-}
