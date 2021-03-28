{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.Method
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudFront.Types.Method
  ( Method
    ( Method'
    , MethodGet
    , MethodHead
    , MethodPost
    , MethodPut
    , MethodPatch
    , MethodOptions
    , MethodDelete
    , fromMethod
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype Method = Method'{fromMethod :: Core.Text}
                   deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                   Core.Generic)
                   deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                     Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON, Core.FromJSON,
                                     Core.ToXML, Core.FromXML, Core.ToText, Core.FromText,
                                     Core.ToByteString, Core.ToQuery, Core.ToHeader)

pattern MethodGet :: Method
pattern MethodGet = Method' "GET"

pattern MethodHead :: Method
pattern MethodHead = Method' "HEAD"

pattern MethodPost :: Method
pattern MethodPost = Method' "POST"

pattern MethodPut :: Method
pattern MethodPut = Method' "PUT"

pattern MethodPatch :: Method
pattern MethodPatch = Method' "PATCH"

pattern MethodOptions :: Method
pattern MethodOptions = Method' "OPTIONS"

pattern MethodDelete :: Method
pattern MethodDelete = Method' "DELETE"

{-# COMPLETE 
  MethodGet,

  MethodHead,

  MethodPost,

  MethodPut,

  MethodPatch,

  MethodOptions,

  MethodDelete,
  Method'
  #-}
