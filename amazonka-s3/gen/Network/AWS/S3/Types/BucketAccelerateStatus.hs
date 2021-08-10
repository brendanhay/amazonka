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
-- Module      : Network.AWS.S3.Types.BucketAccelerateStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.BucketAccelerateStatus
  ( BucketAccelerateStatus
      ( ..,
        BucketAccelerateStatus_Enabled,
        BucketAccelerateStatus_Suspended
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.S3.Internal

newtype BucketAccelerateStatus = BucketAccelerateStatus'
  { fromBucketAccelerateStatus ::
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

pattern BucketAccelerateStatus_Enabled :: BucketAccelerateStatus
pattern BucketAccelerateStatus_Enabled = BucketAccelerateStatus' "Enabled"

pattern BucketAccelerateStatus_Suspended :: BucketAccelerateStatus
pattern BucketAccelerateStatus_Suspended = BucketAccelerateStatus' "Suspended"

{-# COMPLETE
  BucketAccelerateStatus_Enabled,
  BucketAccelerateStatus_Suspended,
  BucketAccelerateStatus'
  #-}
