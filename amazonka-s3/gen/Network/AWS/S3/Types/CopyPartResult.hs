{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.CopyPartResult
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.CopyPartResult where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.S3.Internal

-- | Container for all response elements.
--
-- /See:/ 'newCopyPartResult' smart constructor.
data CopyPartResult = CopyPartResult'
  { -- | Entity tag of the object.
    eTag :: Prelude.Maybe ETag,
    -- | Date and time at which the object was uploaded.
    lastModified :: Prelude.Maybe Prelude.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CopyPartResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eTag', 'copyPartResult_eTag' - Entity tag of the object.
--
-- 'lastModified', 'copyPartResult_lastModified' - Date and time at which the object was uploaded.
newCopyPartResult ::
  CopyPartResult
newCopyPartResult =
  CopyPartResult'
    { eTag = Prelude.Nothing,
      lastModified = Prelude.Nothing
    }

-- | Entity tag of the object.
copyPartResult_eTag :: Lens.Lens' CopyPartResult (Prelude.Maybe ETag)
copyPartResult_eTag = Lens.lens (\CopyPartResult' {eTag} -> eTag) (\s@CopyPartResult' {} a -> s {eTag = a} :: CopyPartResult)

-- | Date and time at which the object was uploaded.
copyPartResult_lastModified :: Lens.Lens' CopyPartResult (Prelude.Maybe Prelude.UTCTime)
copyPartResult_lastModified = Lens.lens (\CopyPartResult' {lastModified} -> lastModified) (\s@CopyPartResult' {} a -> s {lastModified = a} :: CopyPartResult) Prelude.. Lens.mapping Prelude._Time

instance Prelude.FromXML CopyPartResult where
  parseXML x =
    CopyPartResult'
      Prelude.<$> (x Prelude..@? "ETag")
      Prelude.<*> (x Prelude..@? "LastModified")

instance Prelude.Hashable CopyPartResult

instance Prelude.NFData CopyPartResult
