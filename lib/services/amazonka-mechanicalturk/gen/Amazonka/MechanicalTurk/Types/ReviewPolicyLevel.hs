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
-- Module      : Amazonka.MechanicalTurk.Types.ReviewPolicyLevel
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MechanicalTurk.Types.ReviewPolicyLevel
  ( ReviewPolicyLevel
      ( ..,
        ReviewPolicyLevel_Assignment,
        ReviewPolicyLevel_HIT
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype ReviewPolicyLevel = ReviewPolicyLevel'
  { fromReviewPolicyLevel ::
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

pattern ReviewPolicyLevel_Assignment :: ReviewPolicyLevel
pattern ReviewPolicyLevel_Assignment = ReviewPolicyLevel' "Assignment"

pattern ReviewPolicyLevel_HIT :: ReviewPolicyLevel
pattern ReviewPolicyLevel_HIT = ReviewPolicyLevel' "HIT"

{-# COMPLETE
  ReviewPolicyLevel_Assignment,
  ReviewPolicyLevel_HIT,
  ReviewPolicyLevel'
  #-}
