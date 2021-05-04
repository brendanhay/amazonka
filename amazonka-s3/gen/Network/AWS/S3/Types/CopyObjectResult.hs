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
-- Module      : Network.AWS.S3.Types.CopyObjectResult
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.CopyObjectResult where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.S3.Internal

-- | Container for all response elements.
--
-- /See:/ 'newCopyObjectResult' smart constructor.
data CopyObjectResult = CopyObjectResult'
  { -- | Returns the ETag of the new object. The ETag reflects only changes to
    -- the contents of an object, not its metadata. The source and destination
    -- ETag is identical for a successfully copied non-multipart object.
    eTag :: Prelude.Maybe ETag,
    -- | Creation date of the object.
    lastModified :: Prelude.Maybe Prelude.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CopyObjectResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eTag', 'copyObjectResult_eTag' - Returns the ETag of the new object. The ETag reflects only changes to
-- the contents of an object, not its metadata. The source and destination
-- ETag is identical for a successfully copied non-multipart object.
--
-- 'lastModified', 'copyObjectResult_lastModified' - Creation date of the object.
newCopyObjectResult ::
  CopyObjectResult
newCopyObjectResult =
  CopyObjectResult'
    { eTag = Prelude.Nothing,
      lastModified = Prelude.Nothing
    }

-- | Returns the ETag of the new object. The ETag reflects only changes to
-- the contents of an object, not its metadata. The source and destination
-- ETag is identical for a successfully copied non-multipart object.
copyObjectResult_eTag :: Lens.Lens' CopyObjectResult (Prelude.Maybe ETag)
copyObjectResult_eTag = Lens.lens (\CopyObjectResult' {eTag} -> eTag) (\s@CopyObjectResult' {} a -> s {eTag = a} :: CopyObjectResult)

-- | Creation date of the object.
copyObjectResult_lastModified :: Lens.Lens' CopyObjectResult (Prelude.Maybe Prelude.UTCTime)
copyObjectResult_lastModified = Lens.lens (\CopyObjectResult' {lastModified} -> lastModified) (\s@CopyObjectResult' {} a -> s {lastModified = a} :: CopyObjectResult) Prelude.. Lens.mapping Prelude._Time

instance Prelude.FromXML CopyObjectResult where
  parseXML x =
    CopyObjectResult'
      Prelude.<$> (x Prelude..@? "ETag")
      Prelude.<*> (x Prelude..@? "LastModified")

instance Prelude.Hashable CopyObjectResult

instance Prelude.NFData CopyObjectResult
