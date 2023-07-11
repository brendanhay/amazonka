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
-- Module      : Amazonka.Config.Types.ResourceCountGroupKey
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Config.Types.ResourceCountGroupKey
  ( ResourceCountGroupKey
      ( ..,
        ResourceCountGroupKey_ACCOUNT_ID,
        ResourceCountGroupKey_AWS_REGION,
        ResourceCountGroupKey_RESOURCE_TYPE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ResourceCountGroupKey = ResourceCountGroupKey'
  { fromResourceCountGroupKey ::
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
