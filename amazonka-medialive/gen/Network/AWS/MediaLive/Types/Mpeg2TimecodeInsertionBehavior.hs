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
-- Module      : Network.AWS.MediaLive.Types.Mpeg2TimecodeInsertionBehavior
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Mpeg2TimecodeInsertionBehavior
  ( Mpeg2TimecodeInsertionBehavior
      ( ..,
        Mpeg2TimecodeInsertionBehavior_DISABLED,
        Mpeg2TimecodeInsertionBehavior_GOP_TIMECODE
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

-- | Mpeg2 Timecode Insertion Behavior
newtype Mpeg2TimecodeInsertionBehavior = Mpeg2TimecodeInsertionBehavior'
  { fromMpeg2TimecodeInsertionBehavior ::
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

pattern Mpeg2TimecodeInsertionBehavior_DISABLED :: Mpeg2TimecodeInsertionBehavior
pattern Mpeg2TimecodeInsertionBehavior_DISABLED = Mpeg2TimecodeInsertionBehavior' "DISABLED"

pattern Mpeg2TimecodeInsertionBehavior_GOP_TIMECODE :: Mpeg2TimecodeInsertionBehavior
pattern Mpeg2TimecodeInsertionBehavior_GOP_TIMECODE = Mpeg2TimecodeInsertionBehavior' "GOP_TIMECODE"

{-# COMPLETE
  Mpeg2TimecodeInsertionBehavior_DISABLED,
  Mpeg2TimecodeInsertionBehavior_GOP_TIMECODE,
  Mpeg2TimecodeInsertionBehavior'
  #-}
