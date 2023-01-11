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
-- Module      : Amazonka.DynamoDB.Types.StreamViewType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DynamoDB.Types.StreamViewType
  ( StreamViewType
      ( ..,
        StreamViewType_KEYS_ONLY,
        StreamViewType_NEW_AND_OLD_IMAGES,
        StreamViewType_NEW_IMAGE,
        StreamViewType_OLD_IMAGE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import Amazonka.DynamoDB.Types.AttributeValue
import Amazonka.DynamoDB.Types.WriteRequest
import qualified Amazonka.Prelude as Prelude

newtype StreamViewType = StreamViewType'
  { fromStreamViewType ::
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

pattern StreamViewType_KEYS_ONLY :: StreamViewType
pattern StreamViewType_KEYS_ONLY = StreamViewType' "KEYS_ONLY"

pattern StreamViewType_NEW_AND_OLD_IMAGES :: StreamViewType
pattern StreamViewType_NEW_AND_OLD_IMAGES = StreamViewType' "NEW_AND_OLD_IMAGES"

pattern StreamViewType_NEW_IMAGE :: StreamViewType
pattern StreamViewType_NEW_IMAGE = StreamViewType' "NEW_IMAGE"

pattern StreamViewType_OLD_IMAGE :: StreamViewType
pattern StreamViewType_OLD_IMAGE = StreamViewType' "OLD_IMAGE"

{-# COMPLETE
  StreamViewType_KEYS_ONLY,
  StreamViewType_NEW_AND_OLD_IMAGES,
  StreamViewType_NEW_IMAGE,
  StreamViewType_OLD_IMAGE,
  StreamViewType'
  #-}
