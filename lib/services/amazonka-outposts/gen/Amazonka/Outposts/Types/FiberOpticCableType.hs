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
-- Module      : Amazonka.Outposts.Types.FiberOpticCableType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Outposts.Types.FiberOpticCableType
  ( FiberOpticCableType
      ( ..,
        FiberOpticCableType_MULTI_MODE,
        FiberOpticCableType_SINGLE_MODE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype FiberOpticCableType = FiberOpticCableType'
  { fromFiberOpticCableType ::
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

pattern FiberOpticCableType_MULTI_MODE :: FiberOpticCableType
pattern FiberOpticCableType_MULTI_MODE = FiberOpticCableType' "MULTI_MODE"

pattern FiberOpticCableType_SINGLE_MODE :: FiberOpticCableType
pattern FiberOpticCableType_SINGLE_MODE = FiberOpticCableType' "SINGLE_MODE"

{-# COMPLETE
  FiberOpticCableType_MULTI_MODE,
  FiberOpticCableType_SINGLE_MODE,
  FiberOpticCableType'
  #-}
