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
-- Module      : Network.AWS.Route53Resolver.Types.ShareStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53Resolver.Types.ShareStatus
  ( ShareStatus
      ( ..,
        ShareStatus_NOT_SHARED,
        ShareStatus_SHARED_BY_ME,
        ShareStatus_SHARED_WITH_ME
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype ShareStatus = ShareStatus'
  { fromShareStatus ::
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

pattern ShareStatus_NOT_SHARED :: ShareStatus
pattern ShareStatus_NOT_SHARED = ShareStatus' "NOT_SHARED"

pattern ShareStatus_SHARED_BY_ME :: ShareStatus
pattern ShareStatus_SHARED_BY_ME = ShareStatus' "SHARED_BY_ME"

pattern ShareStatus_SHARED_WITH_ME :: ShareStatus
pattern ShareStatus_SHARED_WITH_ME = ShareStatus' "SHARED_WITH_ME"

{-# COMPLETE
  ShareStatus_NOT_SHARED,
  ShareStatus_SHARED_BY_ME,
  ShareStatus_SHARED_WITH_ME,
  ShareStatus'
  #-}
