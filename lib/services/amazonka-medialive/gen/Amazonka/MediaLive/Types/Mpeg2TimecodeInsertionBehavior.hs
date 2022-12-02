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
-- Module      : Amazonka.MediaLive.Types.Mpeg2TimecodeInsertionBehavior
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.Mpeg2TimecodeInsertionBehavior
  ( Mpeg2TimecodeInsertionBehavior
      ( ..,
        Mpeg2TimecodeInsertionBehavior_DISABLED,
        Mpeg2TimecodeInsertionBehavior_GOP_TIMECODE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Mpeg2 Timecode Insertion Behavior
newtype Mpeg2TimecodeInsertionBehavior = Mpeg2TimecodeInsertionBehavior'
  { fromMpeg2TimecodeInsertionBehavior ::
      Data.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
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
