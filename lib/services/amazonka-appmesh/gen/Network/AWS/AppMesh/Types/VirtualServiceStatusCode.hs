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
-- Module      : Network.AWS.AppMesh.Types.VirtualServiceStatusCode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppMesh.Types.VirtualServiceStatusCode
  ( VirtualServiceStatusCode
      ( ..,
        VirtualServiceStatusCode_ACTIVE,
        VirtualServiceStatusCode_DELETED,
        VirtualServiceStatusCode_INACTIVE
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype VirtualServiceStatusCode = VirtualServiceStatusCode'
  { fromVirtualServiceStatusCode ::
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
