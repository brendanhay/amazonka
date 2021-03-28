{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.Types.AssessmentRunNotificationSnsStatusCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Inspector.Types.AssessmentRunNotificationSnsStatusCode
  ( AssessmentRunNotificationSnsStatusCode
    ( AssessmentRunNotificationSnsStatusCode'
    , AssessmentRunNotificationSnsStatusCodeSuccess
    , AssessmentRunNotificationSnsStatusCodeTopicDoesNotExist
    , AssessmentRunNotificationSnsStatusCodeAccessDenied
    , AssessmentRunNotificationSnsStatusCodeInternalError
    , fromAssessmentRunNotificationSnsStatusCode
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype AssessmentRunNotificationSnsStatusCode = AssessmentRunNotificationSnsStatusCode'{fromAssessmentRunNotificationSnsStatusCode
                                                                                         ::
                                                                                         Core.Text}
                                                   deriving stock (Core.Eq, Core.Ord, Core.Read,
                                                                   Core.Show, Core.Generic)
                                                   deriving newtype (Core.IsString, Core.Hashable,
                                                                     Core.NFData, Core.ToJSONKey,
                                                                     Core.FromJSONKey, Core.ToJSON,
                                                                     Core.FromJSON, Core.ToXML,
                                                                     Core.FromXML, Core.ToText,
                                                                     Core.FromText,
                                                                     Core.ToByteString,
                                                                     Core.ToQuery, Core.ToHeader)

pattern AssessmentRunNotificationSnsStatusCodeSuccess :: AssessmentRunNotificationSnsStatusCode
pattern AssessmentRunNotificationSnsStatusCodeSuccess = AssessmentRunNotificationSnsStatusCode' "SUCCESS"

pattern AssessmentRunNotificationSnsStatusCodeTopicDoesNotExist :: AssessmentRunNotificationSnsStatusCode
pattern AssessmentRunNotificationSnsStatusCodeTopicDoesNotExist = AssessmentRunNotificationSnsStatusCode' "TOPIC_DOES_NOT_EXIST"

pattern AssessmentRunNotificationSnsStatusCodeAccessDenied :: AssessmentRunNotificationSnsStatusCode
pattern AssessmentRunNotificationSnsStatusCodeAccessDenied = AssessmentRunNotificationSnsStatusCode' "ACCESS_DENIED"

pattern AssessmentRunNotificationSnsStatusCodeInternalError :: AssessmentRunNotificationSnsStatusCode
pattern AssessmentRunNotificationSnsStatusCodeInternalError = AssessmentRunNotificationSnsStatusCode' "INTERNAL_ERROR"

{-# COMPLETE 
  AssessmentRunNotificationSnsStatusCodeSuccess,

  AssessmentRunNotificationSnsStatusCodeTopicDoesNotExist,

  AssessmentRunNotificationSnsStatusCodeAccessDenied,

  AssessmentRunNotificationSnsStatusCodeInternalError,
  AssessmentRunNotificationSnsStatusCode'
  #-}
