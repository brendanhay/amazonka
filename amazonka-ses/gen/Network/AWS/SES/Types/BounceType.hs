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
-- Module      : Network.AWS.SES.Types.BounceType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SES.Types.BounceType
  ( BounceType
      ( ..,
        BounceType_ContentRejected,
        BounceType_DoesNotExist,
        BounceType_ExceededQuota,
        BounceType_MessageTooLarge,
        BounceType_TemporaryFailure,
        BounceType_Undefined
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype BounceType = BounceType'
  { fromBounceType ::
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

pattern BounceType_ContentRejected :: BounceType
pattern BounceType_ContentRejected = BounceType' "ContentRejected"

pattern BounceType_DoesNotExist :: BounceType
pattern BounceType_DoesNotExist = BounceType' "DoesNotExist"

pattern BounceType_ExceededQuota :: BounceType
pattern BounceType_ExceededQuota = BounceType' "ExceededQuota"

pattern BounceType_MessageTooLarge :: BounceType
pattern BounceType_MessageTooLarge = BounceType' "MessageTooLarge"

pattern BounceType_TemporaryFailure :: BounceType
pattern BounceType_TemporaryFailure = BounceType' "TemporaryFailure"

pattern BounceType_Undefined :: BounceType
pattern BounceType_Undefined = BounceType' "Undefined"

{-# COMPLETE
  BounceType_ContentRejected,
  BounceType_DoesNotExist,
  BounceType_ExceededQuota,
  BounceType_MessageTooLarge,
  BounceType_TemporaryFailure,
  BounceType_Undefined,
  BounceType'
  #-}
