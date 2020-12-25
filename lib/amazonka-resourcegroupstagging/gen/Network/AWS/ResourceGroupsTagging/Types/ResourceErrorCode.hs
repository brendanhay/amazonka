{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
        ResourceErrorCodeInternalServiceException,
        ResourceErrorCodeInvalidParameterException,
        fromResourceErrorCode
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype ResourceErrorCode = ResourceErrorCode'
  { fromResourceErrorCode ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern ResourceErrorCodeInternalServiceException :: ResourceErrorCode
pattern ResourceErrorCodeInternalServiceException = ResourceErrorCode' "InternalServiceException"

pattern ResourceErrorCodeInvalidParameterException :: ResourceErrorCode
pattern ResourceErrorCodeInvalidParameterException = ResourceErrorCode' "InvalidParameterException"

{-# COMPLETE
  ResourceErrorCodeInternalServiceException,
  ResourceErrorCodeInvalidParameterException,
  ResourceErrorCode'
  #-}
