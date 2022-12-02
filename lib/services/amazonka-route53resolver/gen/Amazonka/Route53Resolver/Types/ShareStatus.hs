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
-- Module      : Amazonka.Route53Resolver.Types.ShareStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53Resolver.Types.ShareStatus
  ( ShareStatus
      ( ..,
        ShareStatus_NOT_SHARED,
        ShareStatus_SHARED_BY_ME,
        ShareStatus_SHARED_WITH_ME
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ShareStatus = ShareStatus'
  { fromShareStatus ::
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
