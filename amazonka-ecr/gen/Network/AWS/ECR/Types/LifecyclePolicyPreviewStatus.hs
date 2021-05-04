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
-- Module      : Network.AWS.ECR.Types.LifecyclePolicyPreviewStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECR.Types.LifecyclePolicyPreviewStatus
  ( LifecyclePolicyPreviewStatus
      ( ..,
        LifecyclePolicyPreviewStatus_COMPLETE,
        LifecyclePolicyPreviewStatus_EXPIRED,
        LifecyclePolicyPreviewStatus_FAILED,
        LifecyclePolicyPreviewStatus_IN_PROGRESS
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype LifecyclePolicyPreviewStatus = LifecyclePolicyPreviewStatus'
  { fromLifecyclePolicyPreviewStatus ::
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

pattern LifecyclePolicyPreviewStatus_COMPLETE :: LifecyclePolicyPreviewStatus
pattern LifecyclePolicyPreviewStatus_COMPLETE = LifecyclePolicyPreviewStatus' "COMPLETE"

pattern LifecyclePolicyPreviewStatus_EXPIRED :: LifecyclePolicyPreviewStatus
pattern LifecyclePolicyPreviewStatus_EXPIRED = LifecyclePolicyPreviewStatus' "EXPIRED"

pattern LifecyclePolicyPreviewStatus_FAILED :: LifecyclePolicyPreviewStatus
pattern LifecyclePolicyPreviewStatus_FAILED = LifecyclePolicyPreviewStatus' "FAILED"

pattern LifecyclePolicyPreviewStatus_IN_PROGRESS :: LifecyclePolicyPreviewStatus
pattern LifecyclePolicyPreviewStatus_IN_PROGRESS = LifecyclePolicyPreviewStatus' "IN_PROGRESS"

{-# COMPLETE
  LifecyclePolicyPreviewStatus_COMPLETE,
  LifecyclePolicyPreviewStatus_EXPIRED,
  LifecyclePolicyPreviewStatus_FAILED,
  LifecyclePolicyPreviewStatus_IN_PROGRESS,
  LifecyclePolicyPreviewStatus'
  #-}
