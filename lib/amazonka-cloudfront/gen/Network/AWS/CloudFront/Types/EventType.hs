-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.EventType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.EventType
  ( EventType
      ( EventType',
        OriginRequest,
        OriginResponse,
        ViewerRequest,
        ViewerResponse
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype EventType = EventType' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern OriginRequest :: EventType
pattern OriginRequest = EventType' "origin-request"

pattern OriginResponse :: EventType
pattern OriginResponse = EventType' "origin-response"

pattern ViewerRequest :: EventType
pattern ViewerRequest = EventType' "viewer-request"

pattern ViewerResponse :: EventType
pattern ViewerResponse = EventType' "viewer-response"

{-# COMPLETE
  OriginRequest,
  OriginResponse,
  ViewerRequest,
  ViewerResponse,
  EventType'
  #-}
