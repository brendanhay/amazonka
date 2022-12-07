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
-- Module      : Amazonka.Comprehend.Types.EntityRecognizerEntityList
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Comprehend.Types.EntityRecognizerEntityList where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the entity recognizer submitted with an entity recognizer.
--
-- /See:/ 'newEntityRecognizerEntityList' smart constructor.
data EntityRecognizerEntityList = EntityRecognizerEntityList'
  { -- | Specifies the Amazon S3 location where the entity list is located. The
    -- URI must be in the same region as the API endpoint that you are calling.
    s3Uri :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EntityRecognizerEntityList' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3Uri', 'entityRecognizerEntityList_s3Uri' - Specifies the Amazon S3 location where the entity list is located. The
-- URI must be in the same region as the API endpoint that you are calling.
newEntityRecognizerEntityList ::
  -- | 's3Uri'
  Prelude.Text ->
  EntityRecognizerEntityList
newEntityRecognizerEntityList pS3Uri_ =
  EntityRecognizerEntityList' {s3Uri = pS3Uri_}

-- | Specifies the Amazon S3 location where the entity list is located. The
-- URI must be in the same region as the API endpoint that you are calling.
entityRecognizerEntityList_s3Uri :: Lens.Lens' EntityRecognizerEntityList Prelude.Text
entityRecognizerEntityList_s3Uri = Lens.lens (\EntityRecognizerEntityList' {s3Uri} -> s3Uri) (\s@EntityRecognizerEntityList' {} a -> s {s3Uri = a} :: EntityRecognizerEntityList)

instance Data.FromJSON EntityRecognizerEntityList where
  parseJSON =
    Data.withObject
      "EntityRecognizerEntityList"
      ( \x ->
          EntityRecognizerEntityList'
            Prelude.<$> (x Data..: "S3Uri")
      )

instance Prelude.Hashable EntityRecognizerEntityList where
  hashWithSalt _salt EntityRecognizerEntityList' {..} =
    _salt `Prelude.hashWithSalt` s3Uri

instance Prelude.NFData EntityRecognizerEntityList where
  rnf EntityRecognizerEntityList' {..} =
    Prelude.rnf s3Uri

instance Data.ToJSON EntityRecognizerEntityList where
  toJSON EntityRecognizerEntityList' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("S3Uri" Data..= s3Uri)]
      )
