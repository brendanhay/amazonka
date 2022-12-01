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
-- Module      : Amazonka.LakeFormation.Types.DeleteObjectInput
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LakeFormation.Types.DeleteObjectInput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | An object to delete from the governed table.
--
-- /See:/ 'newDeleteObjectInput' smart constructor.
data DeleteObjectInput = DeleteObjectInput'
  { -- | A list of partition values for the object. A value must be specified for
    -- each partition key associated with the governed table.
    partitionValues :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The Amazon S3 ETag of the object. Returned by @GetTableObjects@ for
    -- validation and used to identify changes to the underlying data.
    eTag :: Prelude.Maybe Prelude.Text,
    -- | The Amazon S3 location of the object to delete.
    uri :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteObjectInput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'partitionValues', 'deleteObjectInput_partitionValues' - A list of partition values for the object. A value must be specified for
-- each partition key associated with the governed table.
--
-- 'eTag', 'deleteObjectInput_eTag' - The Amazon S3 ETag of the object. Returned by @GetTableObjects@ for
-- validation and used to identify changes to the underlying data.
--
-- 'uri', 'deleteObjectInput_uri' - The Amazon S3 location of the object to delete.
newDeleteObjectInput ::
  -- | 'uri'
  Prelude.Text ->
  DeleteObjectInput
newDeleteObjectInput pUri_ =
  DeleteObjectInput'
    { partitionValues =
        Prelude.Nothing,
      eTag = Prelude.Nothing,
      uri = pUri_
    }

-- | A list of partition values for the object. A value must be specified for
-- each partition key associated with the governed table.
deleteObjectInput_partitionValues :: Lens.Lens' DeleteObjectInput (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
deleteObjectInput_partitionValues = Lens.lens (\DeleteObjectInput' {partitionValues} -> partitionValues) (\s@DeleteObjectInput' {} a -> s {partitionValues = a} :: DeleteObjectInput) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon S3 ETag of the object. Returned by @GetTableObjects@ for
-- validation and used to identify changes to the underlying data.
deleteObjectInput_eTag :: Lens.Lens' DeleteObjectInput (Prelude.Maybe Prelude.Text)
deleteObjectInput_eTag = Lens.lens (\DeleteObjectInput' {eTag} -> eTag) (\s@DeleteObjectInput' {} a -> s {eTag = a} :: DeleteObjectInput)

-- | The Amazon S3 location of the object to delete.
deleteObjectInput_uri :: Lens.Lens' DeleteObjectInput Prelude.Text
deleteObjectInput_uri = Lens.lens (\DeleteObjectInput' {uri} -> uri) (\s@DeleteObjectInput' {} a -> s {uri = a} :: DeleteObjectInput)

instance Prelude.Hashable DeleteObjectInput where
  hashWithSalt _salt DeleteObjectInput' {..} =
    _salt `Prelude.hashWithSalt` partitionValues
      `Prelude.hashWithSalt` eTag
      `Prelude.hashWithSalt` uri

instance Prelude.NFData DeleteObjectInput where
  rnf DeleteObjectInput' {..} =
    Prelude.rnf partitionValues
      `Prelude.seq` Prelude.rnf eTag
      `Prelude.seq` Prelude.rnf uri

instance Core.ToJSON DeleteObjectInput where
  toJSON DeleteObjectInput' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("PartitionValues" Core..=)
              Prelude.<$> partitionValues,
            ("ETag" Core..=) Prelude.<$> eTag,
            Prelude.Just ("Uri" Core..= uri)
          ]
      )
