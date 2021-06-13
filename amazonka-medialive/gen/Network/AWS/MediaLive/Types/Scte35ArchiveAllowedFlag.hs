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
-- Module      : Network.AWS.MediaLive.Types.Scte35ArchiveAllowedFlag
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Scte35ArchiveAllowedFlag
  ( Scte35ArchiveAllowedFlag
      ( ..,
        Scte35ArchiveAllowedFlag_ARCHIVE_ALLOWED,
        Scte35ArchiveAllowedFlag_ARCHIVE_NOT_ALLOWED
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

-- | Corresponds to the archive_allowed parameter. A value of
-- ARCHIVE_NOT_ALLOWED corresponds to 0 (false) in the SCTE-35
-- specification. If you include one of the \"restriction\" flags then you
-- must include all four of them.
newtype Scte35ArchiveAllowedFlag = Scte35ArchiveAllowedFlag'
  { fromScte35ArchiveAllowedFlag ::
      Core.Text
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

pattern Scte35ArchiveAllowedFlag_ARCHIVE_ALLOWED :: Scte35ArchiveAllowedFlag
pattern Scte35ArchiveAllowedFlag_ARCHIVE_ALLOWED = Scte35ArchiveAllowedFlag' "ARCHIVE_ALLOWED"

pattern Scte35ArchiveAllowedFlag_ARCHIVE_NOT_ALLOWED :: Scte35ArchiveAllowedFlag
pattern Scte35ArchiveAllowedFlag_ARCHIVE_NOT_ALLOWED = Scte35ArchiveAllowedFlag' "ARCHIVE_NOT_ALLOWED"

{-# COMPLETE
  Scte35ArchiveAllowedFlag_ARCHIVE_ALLOWED,
  Scte35ArchiveAllowedFlag_ARCHIVE_NOT_ALLOWED,
  Scte35ArchiveAllowedFlag'
  #-}
