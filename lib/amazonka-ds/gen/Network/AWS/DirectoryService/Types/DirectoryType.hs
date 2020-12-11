-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.Types.DirectoryType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectoryService.Types.DirectoryType
  ( DirectoryType
      ( DirectoryType',
        ADConnector,
        MicrosoftAD,
        SharedMicrosoftAD,
        SimpleAD
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype DirectoryType = DirectoryType' Lude.Text
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

pattern ADConnector :: DirectoryType
pattern ADConnector = DirectoryType' "ADConnector"

pattern MicrosoftAD :: DirectoryType
pattern MicrosoftAD = DirectoryType' "MicrosoftAD"

pattern SharedMicrosoftAD :: DirectoryType
pattern SharedMicrosoftAD = DirectoryType' "SharedMicrosoftAD"

pattern SimpleAD :: DirectoryType
pattern SimpleAD = DirectoryType' "SimpleAD"

{-# COMPLETE
  ADConnector,
  MicrosoftAD,
  SharedMicrosoftAD,
  SimpleAD,
  DirectoryType'
  #-}
