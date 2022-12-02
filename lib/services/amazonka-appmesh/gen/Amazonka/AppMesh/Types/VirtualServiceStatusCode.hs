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
-- Module      : Amazonka.AppMesh.Types.VirtualServiceStatusCode
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppMesh.Types.VirtualServiceStatusCode
  ( VirtualServiceStatusCode
      ( ..,
        VirtualServiceStatusCode_ACTIVE,
        VirtualServiceStatusCode_DELETED,
        VirtualServiceStatusCode_INACTIVE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype VirtualServiceStatusCode = VirtualServiceStatusCode'
  { fromVirtualServiceStatusCode ::
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

pattern VirtualServiceStatusCode_ACTIVE :: VirtualServiceStatusCode
pattern VirtualServiceStatusCode_ACTIVE = VirtualServiceStatusCode' "ACTIVE"

pattern VirtualServiceStatusCode_DELETED :: VirtualServiceStatusCode
pattern VirtualServiceStatusCode_DELETED = VirtualServiceStatusCode' "DELETED"

pattern VirtualServiceStatusCode_INACTIVE :: VirtualServiceStatusCode
pattern VirtualServiceStatusCode_INACTIVE = VirtualServiceStatusCode' "INACTIVE"

{-# COMPLETE
  VirtualServiceStatusCode_ACTIVE,
  VirtualServiceStatusCode_DELETED,
  VirtualServiceStatusCode_INACTIVE,
  VirtualServiceStatusCode'
  #-}
