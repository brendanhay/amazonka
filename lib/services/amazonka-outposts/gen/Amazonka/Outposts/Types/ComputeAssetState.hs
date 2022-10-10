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
-- Module      : Amazonka.Outposts.Types.ComputeAssetState
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Outposts.Types.ComputeAssetState
  ( ComputeAssetState
      ( ..,
        ComputeAssetState_ACTIVE,
        ComputeAssetState_ISOLATED,
        ComputeAssetState_RETIRING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype ComputeAssetState = ComputeAssetState'
  { fromComputeAssetState ::
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

pattern ComputeAssetState_ACTIVE :: ComputeAssetState
pattern ComputeAssetState_ACTIVE = ComputeAssetState' "ACTIVE"

pattern ComputeAssetState_ISOLATED :: ComputeAssetState
pattern ComputeAssetState_ISOLATED = ComputeAssetState' "ISOLATED"

pattern ComputeAssetState_RETIRING :: ComputeAssetState
pattern ComputeAssetState_RETIRING = ComputeAssetState' "RETIRING"

{-# COMPLETE
  ComputeAssetState_ACTIVE,
  ComputeAssetState_ISOLATED,
  ComputeAssetState_RETIRING,
  ComputeAssetState'
  #-}
