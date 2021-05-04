{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.StreamViewType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.StreamViewType
  ( StreamViewType
      ( ..,
        StreamViewType_KEYS_ONLY,
        StreamViewType_NEW_AND_OLD_IMAGES,
        StreamViewType_NEW_IMAGE,
        StreamViewType_OLD_IMAGE
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype StreamViewType = StreamViewType'
  { fromStreamViewType ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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
