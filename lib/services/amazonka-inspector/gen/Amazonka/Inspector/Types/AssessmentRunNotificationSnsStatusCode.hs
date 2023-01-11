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
-- Module      : Amazonka.Inspector.Types.AssessmentRunNotificationSnsStatusCode
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector.Types.AssessmentRunNotificationSnsStatusCode
  ( AssessmentRunNotificationSnsStatusCode
      ( ..,
        AssessmentRunNotificationSnsStatusCode_ACCESS_DENIED,
        AssessmentRunNotificationSnsStatusCode_INTERNAL_ERROR,
        AssessmentRunNotificationSnsStatusCode_SUCCESS,
        AssessmentRunNotificationSnsStatusCode_TOPIC_DOES_NOT_EXIST
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype AssessmentRunNotificationSnsStatusCode = AssessmentRunNotificationSnsStatusCode'
  { fromAssessmentRunNotificationSnsStatusCode ::
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
