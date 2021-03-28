{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.HttpVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudFront.Types.HttpVersion
  ( HttpVersion
    ( HttpVersion'
    , HttpVersionHTTP1_1
    , HttpVersionHTTP2
    , fromHttpVersion
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype HttpVersion = HttpVersion'{fromHttpVersion :: Core.Text}
                        deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                        Core.Generic)
                        deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                          Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                          Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                          Core.FromText, Core.ToByteString, Core.ToQuery,
                                          Core.ToHeader)

pattern HttpVersionHTTP1_1 :: HttpVersion
pattern HttpVersionHTTP1_1 = HttpVersion' "http1.1"

pattern HttpVersionHTTP2 :: HttpVersion
pattern HttpVersionHTTP2 = HttpVersion' "http2"

{-# COMPLETE 
  HttpVersionHTTP1_1,

  HttpVersionHTTP2,
  HttpVersion'
  #-}
