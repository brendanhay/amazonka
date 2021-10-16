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
-- Module      : Network.AWS.Inspector.Types.AssessmentRunNotificationSnsStatusCode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Inspector.Types.AssessmentRunNotificationSnsStatusCode
  ( AssessmentRunNotificationSnsStatusCode
      ( ..,
        AssessmentRunNotificationSnsStatusCode_ACCESS_DENIED,
        AssessmentRunNotificationSnsStatusCode_INTERNAL_ERROR,
        AssessmentRunNotificationSnsStatusCode_SUCCESS,
        AssessmentRunNotificationSnsStatusCode_TOPIC_DOES_NOT_EXIST
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype AssessmentRunNotificationSnsStatusCode = AssessmentRunNotificationSnsStatusCode'
  { fromAssessmentRunNotificationSnsStatusCode ::
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

pattern AssessmentRunNotificationSnsStatusCode_ACCESS_DENIED :: AssessmentRunNotificationSnsStatusCode
pattern AssessmentRunNotificationSnsStatusCode_ACCESS_DENIED = AssessmentRunNotificationSnsStatusCode' "ACCESS_DENIED"

pattern AssessmentRunNotificationSnsStatusCode_INTERNAL_ERROR :: AssessmentRunNotificationSnsStatusCode
pattern AssessmentRunNotificationSnsStatusCode_INTERNAL_ERROR = AssessmentRunNotificationSnsStatusCode' "INTERNAL_ERROR"

pattern AssessmentRunNotificationSnsStatusCode_SUCCESS :: AssessmentRunNotificationSnsStatusCode
pattern AssessmentRunNotificationSnsStatusCode_SUCCESS = AssessmentRunNotificationSnsStatusCode' "SUCCESS"

pattern AssessmentRunNotificationSnsStatusCode_TOPIC_DOES_NOT_EXIST :: AssessmentRunNotificationSnsStatusCode
pattern AssessmentRunNotificationSnsStatusCode_TOPIC_DOES_NOT_EXIST = AssessmentRunNotificationSnsStatusCode' "TOPIC_DOES_NOT_EXIST"

{-# COMPLETE
  AssessmentRunNotificationSnsStatusCode_ACCESS_DENIED,
  AssessmentRunNotificationSnsStatusCode_INTERNAL_ERROR,
  AssessmentRunNotificationSnsStatusCode_SUCCESS,
  AssessmentRunNotificationSnsStatusCode_TOPIC_DOES_NOT_EXIST,
  AssessmentRunNotificationSnsStatusCode'
  #-}
