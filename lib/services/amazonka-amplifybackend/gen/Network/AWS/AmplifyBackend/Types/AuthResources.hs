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
-- Module      : Network.AWS.AmplifyBackend.Types.AuthResources
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AmplifyBackend.Types.AuthResources
  ( AuthResources
      ( ..,
        AuthResources_IDENTITY_POOL_AND_USER_POOL,
        AuthResources_USER_POOL_ONLY
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype AuthResources = AuthResources'
  { fromAuthResources ::
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

pattern AuthResources_IDENTITY_POOL_AND_USER_POOL :: AuthResources
pattern AuthResources_IDENTITY_POOL_AND_USER_POOL = AuthResources' "IDENTITY_POOL_AND_USER_POOL"

pattern AuthResources_USER_POOL_ONLY :: AuthResources
pattern AuthResources_USER_POOL_ONLY = AuthResources' "USER_POOL_ONLY"

{-# COMPLETE
  AuthResources_IDENTITY_POOL_AND_USER_POOL,
  AuthResources_USER_POOL_ONLY,
  AuthResources'
  #-}
