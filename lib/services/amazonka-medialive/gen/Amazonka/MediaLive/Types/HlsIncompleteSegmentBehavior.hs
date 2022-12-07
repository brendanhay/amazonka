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
-- Module      : Amazonka.MediaLive.Types.HlsIncompleteSegmentBehavior
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.HlsIncompleteSegmentBehavior
  ( HlsIncompleteSegmentBehavior
      ( ..,
        HlsIncompleteSegmentBehavior_AUTO,
        HlsIncompleteSegmentBehavior_SUPPRESS
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Hls Incomplete Segment Behavior
newtype HlsIncompleteSegmentBehavior = HlsIncompleteSegmentBehavior'
  { fromHlsIncompleteSegmentBehavior ::
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

pattern HlsIncompleteSegmentBehavior_AUTO :: HlsIncompleteSegmentBehavior
pattern HlsIncompleteSegmentBehavior_AUTO = HlsIncompleteSegmentBehavior' "AUTO"

pattern HlsIncompleteSegmentBehavior_SUPPRESS :: HlsIncompleteSegmentBehavior
pattern HlsIncompleteSegmentBehavior_SUPPRESS = HlsIncompleteSegmentBehavior' "SUPPRESS"

{-# COMPLETE
  HlsIncompleteSegmentBehavior_AUTO,
  HlsIncompleteSegmentBehavior_SUPPRESS,
  HlsIncompleteSegmentBehavior'
  #-}
