-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Transcribe.Types.RedactionOutput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Transcribe.Types.RedactionOutput
  ( RedactionOutput
      ( RedactionOutput',
        Redacted,
        RedactedAndUnredacted
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype RedactionOutput = RedactionOutput' Lude.Text
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

pattern Redacted :: RedactionOutput
pattern Redacted = RedactionOutput' "redacted"

pattern RedactedAndUnredacted :: RedactionOutput
pattern RedactedAndUnredacted = RedactionOutput' "redacted_and_unredacted"

{-# COMPLETE
  Redacted,
  RedactedAndUnredacted,
  RedactionOutput'
  #-}
