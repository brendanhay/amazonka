{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.RetryBuildBatchType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.RetryBuildBatchType
  ( RetryBuildBatchType
      ( RetryBuildBatchType',
        RetryAllBuilds,
        RetryFailedBuilds
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype RetryBuildBatchType = RetryBuildBatchType' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern RetryAllBuilds :: RetryBuildBatchType
pattern RetryAllBuilds = RetryBuildBatchType' "RETRY_ALL_BUILDS"

pattern RetryFailedBuilds :: RetryBuildBatchType
pattern RetryFailedBuilds = RetryBuildBatchType' "RETRY_FAILED_BUILDS"

{-# COMPLETE
  RetryAllBuilds,
  RetryFailedBuilds,
  RetryBuildBatchType'
  #-}
