-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.Types.AssessmentRunNotificationSNSStatusCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Inspector.Types.AssessmentRunNotificationSNSStatusCode
  ( AssessmentRunNotificationSNSStatusCode
      ( AssessmentRunNotificationSNSStatusCode',
        AccessDenied,
        InternalError,
        Success,
        TopicDoesNotExist
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype AssessmentRunNotificationSNSStatusCode = AssessmentRunNotificationSNSStatusCode' Lude.Text
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

pattern AccessDenied :: AssessmentRunNotificationSNSStatusCode
pattern AccessDenied = AssessmentRunNotificationSNSStatusCode' "ACCESS_DENIED"

pattern InternalError :: AssessmentRunNotificationSNSStatusCode
pattern InternalError = AssessmentRunNotificationSNSStatusCode' "INTERNAL_ERROR"

pattern Success :: AssessmentRunNotificationSNSStatusCode
pattern Success = AssessmentRunNotificationSNSStatusCode' "SUCCESS"

pattern TopicDoesNotExist :: AssessmentRunNotificationSNSStatusCode
pattern TopicDoesNotExist = AssessmentRunNotificationSNSStatusCode' "TOPIC_DOES_NOT_EXIST"

{-# COMPLETE
  AccessDenied,
  InternalError,
  Success,
  TopicDoesNotExist,
  AssessmentRunNotificationSNSStatusCode'
  #-}
