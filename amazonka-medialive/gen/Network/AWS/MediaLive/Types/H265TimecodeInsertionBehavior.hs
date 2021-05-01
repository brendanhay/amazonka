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
-- Module      : Network.AWS.MediaLive.Types.H265TimecodeInsertionBehavior
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.H265TimecodeInsertionBehavior
  ( H265TimecodeInsertionBehavior
      ( ..,
        H265TimecodeInsertionBehavior_DISABLED,
        H265TimecodeInsertionBehavior_PIC_TIMING_SEI
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

-- | H265 Timecode Insertion Behavior
newtype H265TimecodeInsertionBehavior = H265TimecodeInsertionBehavior'
  { fromH265TimecodeInsertionBehavior ::
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

pattern H265TimecodeInsertionBehavior_DISABLED :: H265TimecodeInsertionBehavior
pattern H265TimecodeInsertionBehavior_DISABLED = H265TimecodeInsertionBehavior' "DISABLED"

pattern H265TimecodeInsertionBehavior_PIC_TIMING_SEI :: H265TimecodeInsertionBehavior
pattern H265TimecodeInsertionBehavior_PIC_TIMING_SEI = H265TimecodeInsertionBehavior' "PIC_TIMING_SEI"

{-# COMPLETE
  H265TimecodeInsertionBehavior_DISABLED,
  H265TimecodeInsertionBehavior_PIC_TIMING_SEI,
  H265TimecodeInsertionBehavior'
  #-}
