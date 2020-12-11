-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.DocumentFilterKey
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.DocumentFilterKey
  ( DocumentFilterKey
      ( DocumentFilterKey',
        DFKDocumentType,
        DFKName,
        DFKOwner,
        DFKPlatformTypes
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype DocumentFilterKey = DocumentFilterKey' Lude.Text
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

pattern DFKDocumentType :: DocumentFilterKey
pattern DFKDocumentType = DocumentFilterKey' "DocumentType"

pattern DFKName :: DocumentFilterKey
pattern DFKName = DocumentFilterKey' "Name"

pattern DFKOwner :: DocumentFilterKey
pattern DFKOwner = DocumentFilterKey' "Owner"

pattern DFKPlatformTypes :: DocumentFilterKey
pattern DFKPlatformTypes = DocumentFilterKey' "PlatformTypes"

{-# COMPLETE
  DFKDocumentType,
  DFKName,
  DFKOwner,
  DFKPlatformTypes,
  DocumentFilterKey'
  #-}
