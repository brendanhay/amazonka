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
-- Module      : Network.AWS.CloudFront.Types.EventType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.EventType
  ( EventType
      ( ..,
        EventType_Origin_request,
        EventType_Origin_response,
        EventType_Viewer_request,
        EventType_Viewer_response
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype EventType = EventType'
  { fromEventType ::
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

pattern EventType_Origin_request :: EventType
pattern EventType_Origin_request = EventType' "origin-request"

pattern EventType_Origin_response :: EventType
pattern EventType_Origin_response = EventType' "origin-response"

pattern EventType_Viewer_request :: EventType
pattern EventType_Viewer_request = EventType' "viewer-request"

pattern EventType_Viewer_response :: EventType
pattern EventType_Viewer_response = EventType' "viewer-response"

{-# COMPLETE
  EventType_Origin_request,
  EventType_Origin_response,
  EventType_Viewer_request,
  EventType_Viewer_response,
  EventType'
  #-}
