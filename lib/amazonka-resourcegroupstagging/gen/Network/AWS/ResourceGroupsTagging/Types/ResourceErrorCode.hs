-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ResourceGroupsTagging.Types.ResourceErrorCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ResourceGroupsTagging.Types.ResourceErrorCode
  ( ResourceErrorCode
      ( ResourceErrorCode',
        InternalServiceException,
        InvalidParameterException
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ResourceErrorCode = ResourceErrorCode' Lude.Text
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

pattern InternalServiceException :: ResourceErrorCode
pattern InternalServiceException = ResourceErrorCode' "InternalServiceException"

pattern InvalidParameterException :: ResourceErrorCode
pattern InvalidParameterException = ResourceErrorCode' "InvalidParameterException"

{-# COMPLETE
  InternalServiceException,
  InvalidParameterException,
  ResourceErrorCode'
  #-}
