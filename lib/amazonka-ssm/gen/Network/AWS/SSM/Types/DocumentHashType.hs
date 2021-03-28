{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.DocumentHashType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.DocumentHashType
  ( DocumentHashType
    ( DocumentHashType'
    , DocumentHashTypeHashSHA256
    , DocumentHashTypeHashSHA1
    , fromDocumentHashType
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype DocumentHashType = DocumentHashType'{fromDocumentHashType
                                             :: Core.Text}
                             deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                             Core.Generic)
                             deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                               Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                               Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                               Core.FromText, Core.ToByteString, Core.ToQuery,
                                               Core.ToHeader)

pattern DocumentHashTypeHashSHA256 :: DocumentHashType
pattern DocumentHashTypeHashSHA256 = DocumentHashType' "Sha256"

pattern DocumentHashTypeHashSHA1 :: DocumentHashType
pattern DocumentHashTypeHashSHA1 = DocumentHashType' "Sha1"

{-# COMPLETE 
  DocumentHashTypeHashSHA256,

  DocumentHashTypeHashSHA1,
  DocumentHashType'
  #-}
