{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.OrcCompression
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.OrcCompression
  ( OrcCompression
      ( OrcCompression',
        OCNone,
        OCSnappy,
        OCZlib
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype OrcCompression = OrcCompression' Lude.Text
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

pattern OCNone :: OrcCompression
pattern OCNone = OrcCompression' "NONE"

pattern OCSnappy :: OrcCompression
pattern OCSnappy = OrcCompression' "SNAPPY"

pattern OCZlib :: OrcCompression
pattern OCZlib = OrcCompression' "ZLIB"

{-# COMPLETE
  OCNone,
  OCSnappy,
  OCZlib,
  OrcCompression'
  #-}
