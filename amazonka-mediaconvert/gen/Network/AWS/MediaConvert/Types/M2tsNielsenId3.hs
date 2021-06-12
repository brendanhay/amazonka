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
-- Module      : Network.AWS.MediaConvert.Types.M2tsNielsenId3
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.M2tsNielsenId3
  ( M2tsNielsenId3
      ( ..,
        M2tsNielsenId3_INSERT,
        M2tsNielsenId3_NONE
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | If INSERT, Nielsen inaudible tones for media tracking will be detected
-- in the input audio and an equivalent ID3 tag will be inserted in the
-- output.
newtype M2tsNielsenId3 = M2tsNielsenId3'
  { fromM2tsNielsenId3 ::
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

pattern M2tsNielsenId3_INSERT :: M2tsNielsenId3
pattern M2tsNielsenId3_INSERT = M2tsNielsenId3' "INSERT"

pattern M2tsNielsenId3_NONE :: M2tsNielsenId3
pattern M2tsNielsenId3_NONE = M2tsNielsenId3' "NONE"

{-# COMPLETE
  M2tsNielsenId3_INSERT,
  M2tsNielsenId3_NONE,
  M2tsNielsenId3'
  #-}
