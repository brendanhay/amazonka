-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.FileHeaderInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.FileHeaderInfo
  ( FileHeaderInfo
      ( FileHeaderInfo',
        Ignore,
        None,
        Use
      ),
  )
where

import qualified Network.AWS.Prelude as Lude
import Network.AWS.S3.Internal

newtype FileHeaderInfo = FileHeaderInfo' Lude.Text
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

pattern Ignore :: FileHeaderInfo
pattern Ignore = FileHeaderInfo' "IGNORE"

pattern None :: FileHeaderInfo
pattern None = FileHeaderInfo' "NONE"

pattern Use :: FileHeaderInfo
pattern Use = FileHeaderInfo' "USE"

{-# COMPLETE
  Ignore,
  None,
  Use,
  FileHeaderInfo'
  #-}
