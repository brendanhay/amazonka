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
-- Module      : Network.AWS.LexModels.Types.ContentType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexModels.Types.ContentType
  ( ContentType
      ( ..,
        ContentType_CustomPayload,
        ContentType_PlainText,
        ContentType_SSML
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype ContentType = ContentType'
  { fromContentType ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
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

pattern ContentType_CustomPayload :: ContentType
pattern ContentType_CustomPayload = ContentType' "CustomPayload"

pattern ContentType_PlainText :: ContentType
pattern ContentType_PlainText = ContentType' "PlainText"

pattern ContentType_SSML :: ContentType
pattern ContentType_SSML = ContentType' "SSML"

{-# COMPLETE
  ContentType_CustomPayload,
  ContentType_PlainText,
  ContentType_SSML,
  ContentType'
  #-}
