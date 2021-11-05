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
-- Module      : Amazonka.S3.Types.ArchiveStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.S3.Types.ArchiveStatus
  ( ArchiveStatus
      ( ..,
        ArchiveStatus_ARCHIVE_ACCESS,
        ArchiveStatus_DEEP_ARCHIVE_ACCESS
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude
import Amazonka.S3.Internal

newtype ArchiveStatus = ArchiveStatus'
  { fromArchiveStatus ::
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

pattern ArchiveStatus_ARCHIVE_ACCESS :: ArchiveStatus
pattern ArchiveStatus_ARCHIVE_ACCESS = ArchiveStatus' "ARCHIVE_ACCESS"

pattern ArchiveStatus_DEEP_ARCHIVE_ACCESS :: ArchiveStatus
pattern ArchiveStatus_DEEP_ARCHIVE_ACCESS = ArchiveStatus' "DEEP_ARCHIVE_ACCESS"

{-# COMPLETE
  ArchiveStatus_ARCHIVE_ACCESS,
  ArchiveStatus_DEEP_ARCHIVE_ACCESS,
  ArchiveStatus'
  #-}
