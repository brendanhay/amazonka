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
-- Module      : Amazonka.DynamoDBStreams.Types.StreamViewType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DynamoDBStreams.Types.StreamViewType
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
import Amazonka.DynamoDBStreams.Internal
import qualified Amazonka.Prelude as Prelude

newtype StreamViewType = StreamViewType'
  { fromStreamViewType ::
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
