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
        ArchiveNotAllowed,
        ArchiveAllowed
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Corresponds to the archive_allowed parameter. A value of ARCHIVE_NOT_ALLOWED corresponds to 0 (false) in the SCTE-35 specification. If you include one of the "restriction" flags then you must include all four of them.
newtype Scte35ArchiveAllowedFlag = Scte35ArchiveAllowedFlag' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern ArchiveNotAllowed :: Scte35ArchiveAllowedFlag
pattern ArchiveNotAllowed = Scte35ArchiveAllowedFlag' "ARCHIVE_NOT_ALLOWED"

pattern ArchiveAllowed :: Scte35ArchiveAllowedFlag
pattern ArchiveAllowed = Scte35ArchiveAllowedFlag' "ARCHIVE_ALLOWED"

{-# COMPLETE
  ArchiveNotAllowed,
  ArchiveAllowed,
  Scte35ArchiveAllowedFlag'
  #-}
