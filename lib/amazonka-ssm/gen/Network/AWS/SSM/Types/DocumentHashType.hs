-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.DocumentHashType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.DocumentHashType
  ( DocumentHashType
      ( DocumentHashType',
        HashSHA1,
        HashSHA256
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype DocumentHashType = DocumentHashType' Lude.Text
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

pattern HashSHA1 :: DocumentHashType
pattern HashSHA1 = DocumentHashType' "Sha1"

pattern HashSHA256 :: DocumentHashType
pattern HashSHA256 = DocumentHashType' "Sha256"

{-# COMPLETE
  HashSHA1,
  HashSHA256,
  DocumentHashType'
  #-}
