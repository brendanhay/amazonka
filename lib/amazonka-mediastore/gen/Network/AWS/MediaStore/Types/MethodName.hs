{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaStore.Types.MethodName
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaStore.Types.MethodName
  ( MethodName
    ( MethodName'
    , MethodNamePut
    , MethodNameGet
    , MethodNameDelete
    , MethodNameHead
    , fromMethodName
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype MethodName = MethodName'{fromMethodName :: Core.Text}
                       deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                       Core.Generic)
                       deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                         Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                         Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                         Core.FromText, Core.ToByteString, Core.ToQuery,
                                         Core.ToHeader)

pattern MethodNamePut :: MethodName
pattern MethodNamePut = MethodName' "PUT"

pattern MethodNameGet :: MethodName
pattern MethodNameGet = MethodName' "GET"

pattern MethodNameDelete :: MethodName
pattern MethodNameDelete = MethodName' "DELETE"

pattern MethodNameHead :: MethodName
pattern MethodNameHead = MethodName' "HEAD"

{-# COMPLETE 
  MethodNamePut,

  MethodNameGet,

  MethodNameDelete,

  MethodNameHead,
  MethodName'
  #-}
