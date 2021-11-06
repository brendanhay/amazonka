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
-- Module      : Amazonka.SESV2.Types.ReviewStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SESV2.Types.ReviewStatus
  ( ReviewStatus
      ( ..,
        ReviewStatus_DENIED,
        ReviewStatus_FAILED,
        ReviewStatus_GRANTED,
        ReviewStatus_PENDING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype ReviewStatus = ReviewStatus'
  { fromReviewStatus ::
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

pattern ReviewStatus_DENIED :: ReviewStatus
pattern ReviewStatus_DENIED = ReviewStatus' "DENIED"

pattern ReviewStatus_FAILED :: ReviewStatus
pattern ReviewStatus_FAILED = ReviewStatus' "FAILED"

pattern ReviewStatus_GRANTED :: ReviewStatus
pattern ReviewStatus_GRANTED = ReviewStatus' "GRANTED"

pattern ReviewStatus_PENDING :: ReviewStatus
pattern ReviewStatus_PENDING = ReviewStatus' "PENDING"

{-# COMPLETE
  ReviewStatus_DENIED,
  ReviewStatus_FAILED,
  ReviewStatus_GRANTED,
  ReviewStatus_PENDING,
  ReviewStatus'
  #-}
