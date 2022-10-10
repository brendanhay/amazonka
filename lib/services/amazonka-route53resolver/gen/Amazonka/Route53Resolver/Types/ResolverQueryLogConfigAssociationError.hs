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
-- Module      : Amazonka.Route53Resolver.Types.ResolverQueryLogConfigAssociationError
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53Resolver.Types.ResolverQueryLogConfigAssociationError
  ( ResolverQueryLogConfigAssociationError
      ( ..,
        ResolverQueryLogConfigAssociationError_ACCESS_DENIED,
        ResolverQueryLogConfigAssociationError_DESTINATION_NOT_FOUND,
        ResolverQueryLogConfigAssociationError_INTERNAL_SERVICE_ERROR,
        ResolverQueryLogConfigAssociationError_NONE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype ResolverQueryLogConfigAssociationError = ResolverQueryLogConfigAssociationError'
  { fromResolverQueryLogConfigAssociationError ::
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

pattern ResolverQueryLogConfigAssociationError_ACCESS_DENIED :: ResolverQueryLogConfigAssociationError
pattern ResolverQueryLogConfigAssociationError_ACCESS_DENIED = ResolverQueryLogConfigAssociationError' "ACCESS_DENIED"

pattern ResolverQueryLogConfigAssociationError_DESTINATION_NOT_FOUND :: ResolverQueryLogConfigAssociationError
pattern ResolverQueryLogConfigAssociationError_DESTINATION_NOT_FOUND = ResolverQueryLogConfigAssociationError' "DESTINATION_NOT_FOUND"

pattern ResolverQueryLogConfigAssociationError_INTERNAL_SERVICE_ERROR :: ResolverQueryLogConfigAssociationError
pattern ResolverQueryLogConfigAssociationError_INTERNAL_SERVICE_ERROR = ResolverQueryLogConfigAssociationError' "INTERNAL_SERVICE_ERROR"

pattern ResolverQueryLogConfigAssociationError_NONE :: ResolverQueryLogConfigAssociationError
pattern ResolverQueryLogConfigAssociationError_NONE = ResolverQueryLogConfigAssociationError' "NONE"

{-# COMPLETE
  ResolverQueryLogConfigAssociationError_ACCESS_DENIED,
  ResolverQueryLogConfigAssociationError_DESTINATION_NOT_FOUND,
  ResolverQueryLogConfigAssociationError_INTERNAL_SERVICE_ERROR,
  ResolverQueryLogConfigAssociationError_NONE,
  ResolverQueryLogConfigAssociationError'
  #-}
