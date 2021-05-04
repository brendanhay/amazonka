{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.AuthTokenUpdateStrategyType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.AuthTokenUpdateStrategyType
  ( AuthTokenUpdateStrategyType
      ( ..,
        AuthTokenUpdateStrategyType_DELETE,
        AuthTokenUpdateStrategyType_ROTATE,
        AuthTokenUpdateStrategyType_SET
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype AuthTokenUpdateStrategyType = AuthTokenUpdateStrategyType'
  { fromAuthTokenUpdateStrategyType ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
    )

pattern AuthTokenUpdateStrategyType_DELETE :: AuthTokenUpdateStrategyType
pattern AuthTokenUpdateStrategyType_DELETE = AuthTokenUpdateStrategyType' "DELETE"

pattern AuthTokenUpdateStrategyType_ROTATE :: AuthTokenUpdateStrategyType
pattern AuthTokenUpdateStrategyType_ROTATE = AuthTokenUpdateStrategyType' "ROTATE"

pattern AuthTokenUpdateStrategyType_SET :: AuthTokenUpdateStrategyType
pattern AuthTokenUpdateStrategyType_SET = AuthTokenUpdateStrategyType' "SET"

{-# COMPLETE
  AuthTokenUpdateStrategyType_DELETE,
  AuthTokenUpdateStrategyType_ROTATE,
  AuthTokenUpdateStrategyType_SET,
  AuthTokenUpdateStrategyType'
  #-}
