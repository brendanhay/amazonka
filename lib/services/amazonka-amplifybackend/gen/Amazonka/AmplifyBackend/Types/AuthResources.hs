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
-- Module      : Amazonka.AmplifyBackend.Types.AuthResources
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AmplifyBackend.Types.AuthResources
  ( AuthResources
      ( ..,
        AuthResources_IDENTITY_POOL_AND_USER_POOL,
        AuthResources_USER_POOL_ONLY
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype AuthResources = AuthResources'
  { fromAuthResources ::
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

pattern AuthResources_IDENTITY_POOL_AND_USER_POOL :: AuthResources
pattern AuthResources_IDENTITY_POOL_AND_USER_POOL = AuthResources' "IDENTITY_POOL_AND_USER_POOL"

pattern AuthResources_USER_POOL_ONLY :: AuthResources
pattern AuthResources_USER_POOL_ONLY = AuthResources' "USER_POOL_ONLY"

{-# COMPLETE
  AuthResources_IDENTITY_POOL_AND_USER_POOL,
  AuthResources_USER_POOL_ONLY,
  AuthResources'
  #-}
