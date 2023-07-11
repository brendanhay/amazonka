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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ComputeAssetState = ComputeAssetState'
  { fromComputeAssetState ::
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
