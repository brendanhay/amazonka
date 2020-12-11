-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.Types.StatusCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatch.Types.StatusCode
  ( StatusCode
      ( StatusCode',
        Complete,
        InternalError,
        PartialData
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype StatusCode = StatusCode' Lude.Text
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

pattern Complete :: StatusCode
pattern Complete = StatusCode' "Complete"

pattern InternalError :: StatusCode
pattern InternalError = StatusCode' "InternalError"

pattern PartialData :: StatusCode
pattern PartialData = StatusCode' "PartialData"

{-# COMPLETE
  Complete,
  InternalError,
  PartialData,
  StatusCode'
  #-}
