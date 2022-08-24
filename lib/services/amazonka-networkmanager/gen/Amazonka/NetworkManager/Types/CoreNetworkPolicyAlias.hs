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
-- Module      : Amazonka.NetworkManager.Types.CoreNetworkPolicyAlias
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.NetworkManager.Types.CoreNetworkPolicyAlias
  ( CoreNetworkPolicyAlias
      ( ..,
        CoreNetworkPolicyAlias_LATEST,
        CoreNetworkPolicyAlias_LIVE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype CoreNetworkPolicyAlias = CoreNetworkPolicyAlias'
  { fromCoreNetworkPolicyAlias ::
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

pattern CoreNetworkPolicyAlias_LATEST :: CoreNetworkPolicyAlias
pattern CoreNetworkPolicyAlias_LATEST = CoreNetworkPolicyAlias' "LATEST"

pattern CoreNetworkPolicyAlias_LIVE :: CoreNetworkPolicyAlias
pattern CoreNetworkPolicyAlias_LIVE = CoreNetworkPolicyAlias' "LIVE"

{-# COMPLETE
  CoreNetworkPolicyAlias_LATEST,
  CoreNetworkPolicyAlias_LIVE,
  CoreNetworkPolicyAlias'
  #-}
