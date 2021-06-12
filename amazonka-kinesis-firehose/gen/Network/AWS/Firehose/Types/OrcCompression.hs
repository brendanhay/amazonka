{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.OrcCompression
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.OrcCompression
  ( OrcCompression
      ( ..,
        OrcCompression_NONE,
        OrcCompression_SNAPPY,
        OrcCompression_ZLIB
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype OrcCompression = OrcCompression'
  { fromOrcCompression ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
    )

pattern OrcCompression_NONE :: OrcCompression
pattern OrcCompression_NONE = OrcCompression' "NONE"

pattern OrcCompression_SNAPPY :: OrcCompression
pattern OrcCompression_SNAPPY = OrcCompression' "SNAPPY"

pattern OrcCompression_ZLIB :: OrcCompression
pattern OrcCompression_ZLIB = OrcCompression' "ZLIB"

{-# COMPLETE
  OrcCompression_NONE,
  OrcCompression_SNAPPY,
  OrcCompression_ZLIB,
  OrcCompression'
  #-}
