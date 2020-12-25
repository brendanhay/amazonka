{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.AuthTokenUpdateStrategyType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.AuthTokenUpdateStrategyType
  ( AuthTokenUpdateStrategyType
      ( AuthTokenUpdateStrategyType',
        AuthTokenUpdateStrategyTypeSet,
        AuthTokenUpdateStrategyTypeRotate,
        AuthTokenUpdateStrategyTypeDelete,
        fromAuthTokenUpdateStrategyType
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype AuthTokenUpdateStrategyType = AuthTokenUpdateStrategyType'
  { fromAuthTokenUpdateStrategyType ::
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

pattern AuthTokenUpdateStrategyTypeSet :: AuthTokenUpdateStrategyType
pattern AuthTokenUpdateStrategyTypeSet = AuthTokenUpdateStrategyType' "SET"

pattern AuthTokenUpdateStrategyTypeRotate :: AuthTokenUpdateStrategyType
pattern AuthTokenUpdateStrategyTypeRotate = AuthTokenUpdateStrategyType' "ROTATE"

pattern AuthTokenUpdateStrategyTypeDelete :: AuthTokenUpdateStrategyType
pattern AuthTokenUpdateStrategyTypeDelete = AuthTokenUpdateStrategyType' "DELETE"

{-# COMPLETE
  AuthTokenUpdateStrategyTypeSet,
  AuthTokenUpdateStrategyTypeRotate,
  AuthTokenUpdateStrategyTypeDelete,
  AuthTokenUpdateStrategyType'
  #-}
