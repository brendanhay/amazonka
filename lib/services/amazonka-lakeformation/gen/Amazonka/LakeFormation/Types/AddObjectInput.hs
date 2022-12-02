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
-- Module      : Amazonka.LakeFormation.Types.AddObjectInput
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LakeFormation.Types.AddObjectInput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A new object to add to the governed table.
--
-- /See:/ 'newAddObjectInput' smart constructor.
data AddObjectInput = AddObjectInput'
  { -- | A list of partition values for the object. A value must be specified for
    -- each partition key associated with the table.
    --
    -- The supported data types are integer, long, date(yyyy-MM-dd),
    -- timestamp(yyyy-MM-dd HH:mm:ssXXX or yyyy-MM-dd HH:mm:ss\"), string and
    -- decimal.
    partitionValues :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The Amazon S3 location of the object.
    uri :: Prelude.Text,
    -- | The Amazon S3 ETag of the object. Returned by @GetTableObjects@ for
    -- validation and used to identify changes to the underlying data.
    eTag :: Prelude.Text,
    -- | The size of the Amazon S3 object in bytes.
    size :: Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AddObjectInput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'partitionValues', 'addObjectInput_partitionValues' - A list of partition values for the object. A value must be specified for
-- each partition key associated with the table.
--
-- The supported data types are integer, long, date(yyyy-MM-dd),
-- timestamp(yyyy-MM-dd HH:mm:ssXXX or yyyy-MM-dd HH:mm:ss\"), string and
-- decimal.
--
-- 'uri', 'addObjectInput_uri' - The Amazon S3 location of the object.
--
-- 'eTag', 'addObjectInput_eTag' - The Amazon S3 ETag of the object. Returned by @GetTableObjects@ for
-- validation and used to identify changes to the underlying data.
--
-- 'size', 'addObjectInput_size' - The size of the Amazon S3 object in bytes.
newAddObjectInput ::
  -- | 'uri'
  Prelude.Text ->
  -- | 'eTag'
  Prelude.Text ->
  -- | 'size'
  Prelude.Integer ->
  AddObjectInput
newAddObjectInput pUri_ pETag_ pSize_ =
  AddObjectInput'
    { partitionValues = Prelude.Nothing,
      uri = pUri_,
      eTag = pETag_,
      size = pSize_
    }

-- | A list of partition values for the object. A value must be specified for
-- each partition key associated with the table.
--
-- The supported data types are integer, long, date(yyyy-MM-dd),
-- timestamp(yyyy-MM-dd HH:mm:ssXXX or yyyy-MM-dd HH:mm:ss\"), string and
-- decimal.
addObjectInput_partitionValues :: Lens.Lens' AddObjectInput (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
addObjectInput_partitionValues = Lens.lens (\AddObjectInput' {partitionValues} -> partitionValues) (\s@AddObjectInput' {} a -> s {partitionValues = a} :: AddObjectInput) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon S3 location of the object.
addObjectInput_uri :: Lens.Lens' AddObjectInput Prelude.Text
addObjectInput_uri = Lens.lens (\AddObjectInput' {uri} -> uri) (\s@AddObjectInput' {} a -> s {uri = a} :: AddObjectInput)

-- | The Amazon S3 ETag of the object. Returned by @GetTableObjects@ for
-- validation and used to identify changes to the underlying data.
addObjectInput_eTag :: Lens.Lens' AddObjectInput Prelude.Text
addObjectInput_eTag = Lens.lens (\AddObjectInput' {eTag} -> eTag) (\s@AddObjectInput' {} a -> s {eTag = a} :: AddObjectInput)

-- | The size of the Amazon S3 object in bytes.
addObjectInput_size :: Lens.Lens' AddObjectInput Prelude.Integer
addObjectInput_size = Lens.lens (\AddObjectInput' {size} -> size) (\s@AddObjectInput' {} a -> s {size = a} :: AddObjectInput)

instance Prelude.Hashable AddObjectInput where
  hashWithSalt _salt AddObjectInput' {..} =
    _salt `Prelude.hashWithSalt` partitionValues
      `Prelude.hashWithSalt` uri
      `Prelude.hashWithSalt` eTag
      `Prelude.hashWithSalt` size

instance Prelude.NFData AddObjectInput where
  rnf AddObjectInput' {..} =
    Prelude.rnf partitionValues
      `Prelude.seq` Prelude.rnf uri
      `Prelude.seq` Prelude.rnf eTag
      `Prelude.seq` Prelude.rnf size

instance Data.ToJSON AddObjectInput where
  toJSON AddObjectInput' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("PartitionValues" Data..=)
              Prelude.<$> partitionValues,
            Prelude.Just ("Uri" Data..= uri),
            Prelude.Just ("ETag" Data..= eTag),
            Prelude.Just ("Size" Data..= size)
          ]
      )
