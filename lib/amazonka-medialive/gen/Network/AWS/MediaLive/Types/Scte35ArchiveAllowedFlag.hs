{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Scte35ArchiveAllowedFlag
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Scte35ArchiveAllowedFlag
  ( Scte35ArchiveAllowedFlag
      ( Scte35ArchiveAllowedFlag',
        Scte35ArchiveAllowedFlagArchiveNotAllowed,
        Scte35ArchiveAllowedFlagArchiveAllowed,
        fromScte35ArchiveAllowedFlag
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | Corresponds to the archive_allowed parameter. A value of ARCHIVE_NOT_ALLOWED corresponds to 0 (false) in the SCTE-35 specification. If you include one of the "restriction" flags then you must include all four of them.
newtype Scte35ArchiveAllowedFlag = Scte35ArchiveAllowedFlag'
  { fromScte35ArchiveAllowedFlag ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern Scte35ArchiveAllowedFlagArchiveNotAllowed :: Scte35ArchiveAllowedFlag
pattern Scte35ArchiveAllowedFlagArchiveNotAllowed = Scte35ArchiveAllowedFlag' "ARCHIVE_NOT_ALLOWED"

pattern Scte35ArchiveAllowedFlagArchiveAllowed :: Scte35ArchiveAllowedFlag
pattern Scte35ArchiveAllowedFlagArchiveAllowed = Scte35ArchiveAllowedFlag' "ARCHIVE_ALLOWED"

{-# COMPLETE
  Scte35ArchiveAllowedFlagArchiveNotAllowed,
  Scte35ArchiveAllowedFlagArchiveAllowed,
  Scte35ArchiveAllowedFlag'
  #-}
