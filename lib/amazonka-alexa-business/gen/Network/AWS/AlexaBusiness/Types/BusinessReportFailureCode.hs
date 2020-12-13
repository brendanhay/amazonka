{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.BusinessReportFailureCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.BusinessReportFailureCode
  ( BusinessReportFailureCode
      ( BusinessReportFailureCode',
        AccessDenied,
        NoSuchBucket,
        InternalFailure
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype BusinessReportFailureCode = BusinessReportFailureCode' Lude.Text
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

pattern AccessDenied :: BusinessReportFailureCode
pattern AccessDenied = BusinessReportFailureCode' "ACCESS_DENIED"

pattern NoSuchBucket :: BusinessReportFailureCode
pattern NoSuchBucket = BusinessReportFailureCode' "NO_SUCH_BUCKET"

pattern InternalFailure :: BusinessReportFailureCode
pattern InternalFailure = BusinessReportFailureCode' "INTERNAL_FAILURE"

{-# COMPLETE
  AccessDenied,
  NoSuchBucket,
  InternalFailure,
  BusinessReportFailureCode'
  #-}
