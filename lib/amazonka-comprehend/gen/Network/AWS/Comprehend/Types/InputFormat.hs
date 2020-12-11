-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.InputFormat
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.InputFormat
  ( InputFormat
      ( InputFormat',
        OneDocPerFile,
        OneDocPerLine
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype InputFormat = InputFormat' Lude.Text
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

pattern OneDocPerFile :: InputFormat
pattern OneDocPerFile = InputFormat' "ONE_DOC_PER_FILE"

pattern OneDocPerLine :: InputFormat
pattern OneDocPerLine = InputFormat' "ONE_DOC_PER_LINE"

{-# COMPLETE
  OneDocPerFile,
  OneDocPerLine,
  InputFormat'
  #-}
