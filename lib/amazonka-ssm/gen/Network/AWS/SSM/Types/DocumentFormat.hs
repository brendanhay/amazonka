-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.DocumentFormat
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.DocumentFormat
  ( DocumentFormat
      ( DocumentFormat',
        JSON,
        Text,
        Yaml
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype DocumentFormat = DocumentFormat' Lude.Text
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

pattern JSON :: DocumentFormat
pattern JSON = DocumentFormat' "JSON"

pattern Text :: DocumentFormat
pattern Text = DocumentFormat' "TEXT"

pattern Yaml :: DocumentFormat
pattern Yaml = DocumentFormat' "YAML"

{-# COMPLETE
  JSON,
  Text,
  Yaml,
  DocumentFormat'
  #-}
