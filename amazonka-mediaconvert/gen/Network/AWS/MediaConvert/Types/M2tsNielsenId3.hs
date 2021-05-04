{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
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

import qualified Network.AWS.Prelude as Prelude

-- | If INSERT, Nielsen inaudible tones for media tracking will be detected
-- in the input audio and an equivalent ID3 tag will be inserted in the
-- output.
newtype M2tsNielsenId3 = M2tsNielsenId3'
  { fromM2tsNielsenId3 ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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
