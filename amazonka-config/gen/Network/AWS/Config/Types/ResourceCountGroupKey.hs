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
-- Module      : Network.AWS.Config.Types.ResourceCountGroupKey
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.ResourceCountGroupKey
  ( ResourceCountGroupKey
      ( ..,
        ResourceCountGroupKey_ACCOUNT_ID,
        ResourceCountGroupKey_AWS_REGION,
        ResourceCountGroupKey_RESOURCE_TYPE
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype ResourceCountGroupKey = ResourceCountGroupKey'
  { fromResourceCountGroupKey ::
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

pattern ResourceCountGroupKey_ACCOUNT_ID :: ResourceCountGroupKey
pattern ResourceCountGroupKey_ACCOUNT_ID = ResourceCountGroupKey' "ACCOUNT_ID"

pattern ResourceCountGroupKey_AWS_REGION :: ResourceCountGroupKey
pattern ResourceCountGroupKey_AWS_REGION = ResourceCountGroupKey' "AWS_REGION"

pattern ResourceCountGroupKey_RESOURCE_TYPE :: ResourceCountGroupKey
pattern ResourceCountGroupKey_RESOURCE_TYPE = ResourceCountGroupKey' "RESOURCE_TYPE"

{-# COMPLETE
  ResourceCountGroupKey_ACCOUNT_ID,
  ResourceCountGroupKey_AWS_REGION,
  ResourceCountGroupKey_RESOURCE_TYPE,
  ResourceCountGroupKey'
  #-}
