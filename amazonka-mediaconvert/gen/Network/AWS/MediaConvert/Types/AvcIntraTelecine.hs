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
-- Module      : Network.AWS.MediaConvert.Types.AvcIntraTelecine
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.AvcIntraTelecine
  ( AvcIntraTelecine
      ( ..,
        AvcIntraTelecine_HARD,
        AvcIntraTelecine_NONE
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

-- | When you do frame rate conversion from 23.976 frames per second (fps) to
-- 29.97 fps, and your output scan type is interlaced, you can optionally
-- enable hard telecine (HARD) to create a smoother picture. When you keep
-- the default value, None (NONE), MediaConvert does a standard frame rate
-- conversion to 29.97 without doing anything with the field polarity to
-- create a smoother picture.
newtype AvcIntraTelecine = AvcIntraTelecine'
  { fromAvcIntraTelecine ::
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

pattern AvcIntraTelecine_HARD :: AvcIntraTelecine
pattern AvcIntraTelecine_HARD = AvcIntraTelecine' "HARD"

pattern AvcIntraTelecine_NONE :: AvcIntraTelecine
pattern AvcIntraTelecine_NONE = AvcIntraTelecine' "NONE"

{-# COMPLETE
  AvcIntraTelecine_HARD,
  AvcIntraTelecine_NONE,
  AvcIntraTelecine'
  #-}
