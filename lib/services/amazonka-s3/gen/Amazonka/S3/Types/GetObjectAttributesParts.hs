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
-- Module      : Amazonka.S3.Types.GetObjectAttributesParts
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.S3.Types.GetObjectAttributesParts where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.S3.Internal
import Amazonka.S3.Types.ObjectPart

-- | A collection of parts associated with a multipart upload.
--
-- /See:/ 'newGetObjectAttributesParts' smart constructor.
data GetObjectAttributesParts = GetObjectAttributesParts'
  { -- | Indicates whether the returned list of parts is truncated. A value of
    -- @true@ indicates that the list was truncated. A list can be truncated if
    -- the number of parts exceeds the limit returned in the @MaxParts@
    -- element.
    isTruncated :: Prelude.Maybe Prelude.Bool,
    -- | The maximum number of parts allowed in the response.
    maxParts :: Prelude.Maybe Prelude.Int,
    -- | When a list is truncated, this element specifies the last part in the
    -- list, as well as the value to use for the @PartNumberMarker@ request
    -- parameter in a subsequent request.
    nextPartNumberMarker :: Prelude.Maybe Prelude.Int,
    -- | The marker for the current part.
    partNumberMarker :: Prelude.Maybe Prelude.Int,
    -- | A container for elements related to a particular part. A response can
    -- contain zero or more @Parts@ elements.
    parts :: Prelude.Maybe [ObjectPart],
    -- | The total number of parts.
    totalPartsCount :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetObjectAttributesParts' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'isTruncated', 'getObjectAttributesParts_isTruncated' - Indicates whether the returned list of parts is truncated. A value of
-- @true@ indicates that the list was truncated. A list can be truncated if
-- the number of parts exceeds the limit returned in the @MaxParts@
-- element.
--
-- 'maxParts', 'getObjectAttributesParts_maxParts' - The maximum number of parts allowed in the response.
--
-- 'nextPartNumberMarker', 'getObjectAttributesParts_nextPartNumberMarker' - When a list is truncated, this element specifies the last part in the
-- list, as well as the value to use for the @PartNumberMarker@ request
-- parameter in a subsequent request.
--
-- 'partNumberMarker', 'getObjectAttributesParts_partNumberMarker' - The marker for the current part.
--
-- 'parts', 'getObjectAttributesParts_parts' - A container for elements related to a particular part. A response can
-- contain zero or more @Parts@ elements.
--
-- 'totalPartsCount', 'getObjectAttributesParts_totalPartsCount' - The total number of parts.
newGetObjectAttributesParts ::
  GetObjectAttributesParts
newGetObjectAttributesParts =
  GetObjectAttributesParts'
    { isTruncated =
        Prelude.Nothing,
      maxParts = Prelude.Nothing,
      nextPartNumberMarker = Prelude.Nothing,
      partNumberMarker = Prelude.Nothing,
      parts = Prelude.Nothing,
      totalPartsCount = Prelude.Nothing
    }

-- | Indicates whether the returned list of parts is truncated. A value of
-- @true@ indicates that the list was truncated. A list can be truncated if
-- the number of parts exceeds the limit returned in the @MaxParts@
-- element.
getObjectAttributesParts_isTruncated :: Lens.Lens' GetObjectAttributesParts (Prelude.Maybe Prelude.Bool)
getObjectAttributesParts_isTruncated = Lens.lens (\GetObjectAttributesParts' {isTruncated} -> isTruncated) (\s@GetObjectAttributesParts' {} a -> s {isTruncated = a} :: GetObjectAttributesParts)

-- | The maximum number of parts allowed in the response.
getObjectAttributesParts_maxParts :: Lens.Lens' GetObjectAttributesParts (Prelude.Maybe Prelude.Int)
getObjectAttributesParts_maxParts = Lens.lens (\GetObjectAttributesParts' {maxParts} -> maxParts) (\s@GetObjectAttributesParts' {} a -> s {maxParts = a} :: GetObjectAttributesParts)

-- | When a list is truncated, this element specifies the last part in the
-- list, as well as the value to use for the @PartNumberMarker@ request
-- parameter in a subsequent request.
getObjectAttributesParts_nextPartNumberMarker :: Lens.Lens' GetObjectAttributesParts (Prelude.Maybe Prelude.Int)
getObjectAttributesParts_nextPartNumberMarker = Lens.lens (\GetObjectAttributesParts' {nextPartNumberMarker} -> nextPartNumberMarker) (\s@GetObjectAttributesParts' {} a -> s {nextPartNumberMarker = a} :: GetObjectAttributesParts)

-- | The marker for the current part.
getObjectAttributesParts_partNumberMarker :: Lens.Lens' GetObjectAttributesParts (Prelude.Maybe Prelude.Int)
getObjectAttributesParts_partNumberMarker = Lens.lens (\GetObjectAttributesParts' {partNumberMarker} -> partNumberMarker) (\s@GetObjectAttributesParts' {} a -> s {partNumberMarker = a} :: GetObjectAttributesParts)

-- | A container for elements related to a particular part. A response can
-- contain zero or more @Parts@ elements.
getObjectAttributesParts_parts :: Lens.Lens' GetObjectAttributesParts (Prelude.Maybe [ObjectPart])
getObjectAttributesParts_parts = Lens.lens (\GetObjectAttributesParts' {parts} -> parts) (\s@GetObjectAttributesParts' {} a -> s {parts = a} :: GetObjectAttributesParts) Prelude.. Lens.mapping Lens.coerced

-- | The total number of parts.
getObjectAttributesParts_totalPartsCount :: Lens.Lens' GetObjectAttributesParts (Prelude.Maybe Prelude.Int)
getObjectAttributesParts_totalPartsCount = Lens.lens (\GetObjectAttributesParts' {totalPartsCount} -> totalPartsCount) (\s@GetObjectAttributesParts' {} a -> s {totalPartsCount = a} :: GetObjectAttributesParts)

instance Data.FromXML GetObjectAttributesParts where
  parseXML x =
    GetObjectAttributesParts'
      Prelude.<$> (x Data..@? "IsTruncated")
      Prelude.<*> (x Data..@? "MaxParts")
      Prelude.<*> (x Data..@? "NextPartNumberMarker")
      Prelude.<*> (x Data..@? "PartNumberMarker")
      Prelude.<*> (Core.may (Data.parseXMLList "Part") x)
      Prelude.<*> (x Data..@? "PartsCount")

instance Prelude.Hashable GetObjectAttributesParts where
  hashWithSalt _salt GetObjectAttributesParts' {..} =
    _salt
      `Prelude.hashWithSalt` isTruncated
      `Prelude.hashWithSalt` maxParts
      `Prelude.hashWithSalt` nextPartNumberMarker
      `Prelude.hashWithSalt` partNumberMarker
      `Prelude.hashWithSalt` parts
      `Prelude.hashWithSalt` totalPartsCount

instance Prelude.NFData GetObjectAttributesParts where
  rnf GetObjectAttributesParts' {..} =
    Prelude.rnf isTruncated
      `Prelude.seq` Prelude.rnf maxParts
      `Prelude.seq` Prelude.rnf nextPartNumberMarker
      `Prelude.seq` Prelude.rnf partNumberMarker
      `Prelude.seq` Prelude.rnf parts
      `Prelude.seq` Prelude.rnf totalPartsCount
