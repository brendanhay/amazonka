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
-- Module      : Network.AWS.Comprehend.Types.EntityRecognizerEntityList
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.EntityRecognizerEntityList where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes the entity recognizer submitted with an entity recognizer.
--
-- /See:/ 'newEntityRecognizerEntityList' smart constructor.
data EntityRecognizerEntityList = EntityRecognizerEntityList'
  { -- | Specifies the Amazon S3 location where the entity list is located. The
    -- URI must be in the same region as the API endpoint that you are calling.
    s3Uri :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.FromJSON EntityRecognizerEntityList where
  parseJSON =
    Prelude.withObject
      "EntityRecognizerEntityList"
      ( \x ->
          EntityRecognizerEntityList'
            Prelude.<$> (x Prelude..: "S3Uri")
      )

instance Prelude.Hashable EntityRecognizerEntityList

instance Prelude.NFData EntityRecognizerEntityList

instance Prelude.ToJSON EntityRecognizerEntityList where
  toJSON EntityRecognizerEntityList' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("S3Uri" Prelude..= s3Uri)]
      )
