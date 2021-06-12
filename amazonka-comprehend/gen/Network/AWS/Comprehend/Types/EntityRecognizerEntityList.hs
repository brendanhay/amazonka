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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Describes the entity recognizer submitted with an entity recognizer.
--
-- /See:/ 'newEntityRecognizerEntityList' smart constructor.
data EntityRecognizerEntityList = EntityRecognizerEntityList'
  { -- | Specifies the Amazon S3 location where the entity list is located. The
    -- URI must be in the same region as the API endpoint that you are calling.
    s3Uri :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  EntityRecognizerEntityList
newEntityRecognizerEntityList pS3Uri_ =
  EntityRecognizerEntityList' {s3Uri = pS3Uri_}

-- | Specifies the Amazon S3 location where the entity list is located. The
-- URI must be in the same region as the API endpoint that you are calling.
entityRecognizerEntityList_s3Uri :: Lens.Lens' EntityRecognizerEntityList Core.Text
entityRecognizerEntityList_s3Uri = Lens.lens (\EntityRecognizerEntityList' {s3Uri} -> s3Uri) (\s@EntityRecognizerEntityList' {} a -> s {s3Uri = a} :: EntityRecognizerEntityList)

instance Core.FromJSON EntityRecognizerEntityList where
  parseJSON =
    Core.withObject
      "EntityRecognizerEntityList"
      ( \x ->
          EntityRecognizerEntityList'
            Core.<$> (x Core..: "S3Uri")
      )

instance Core.Hashable EntityRecognizerEntityList

instance Core.NFData EntityRecognizerEntityList

instance Core.ToJSON EntityRecognizerEntityList where
  toJSON EntityRecognizerEntityList' {..} =
    Core.object
      (Core.catMaybes [Core.Just ("S3Uri" Core..= s3Uri)])
