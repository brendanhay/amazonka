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
-- Module      : Network.AWS.Rekognition.Types.Instance
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.Instance where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Rekognition.Types.BoundingBox

-- | An instance of a label returned by Amazon Rekognition Image
-- (DetectLabels) or by Amazon Rekognition Video (GetLabelDetection).
--
-- /See:/ 'newInstance' smart constructor.
data Instance = Instance'
  { -- | The position of the label instance on the image.
    boundingBox :: Prelude.Maybe BoundingBox,
    -- | The confidence that Amazon Rekognition has in the accuracy of the
    -- bounding box.
    confidence :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Instance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'boundingBox', 'instance_boundingBox' - The position of the label instance on the image.
--
-- 'confidence', 'instance_confidence' - The confidence that Amazon Rekognition has in the accuracy of the
-- bounding box.
newInstance ::
  Instance
newInstance =
  Instance'
    { boundingBox = Prelude.Nothing,
      confidence = Prelude.Nothing
    }

-- | The position of the label instance on the image.
instance_boundingBox :: Lens.Lens' Instance (Prelude.Maybe BoundingBox)
instance_boundingBox = Lens.lens (\Instance' {boundingBox} -> boundingBox) (\s@Instance' {} a -> s {boundingBox = a} :: Instance)

-- | The confidence that Amazon Rekognition has in the accuracy of the
-- bounding box.
instance_confidence :: Lens.Lens' Instance (Prelude.Maybe Prelude.Double)
instance_confidence = Lens.lens (\Instance' {confidence} -> confidence) (\s@Instance' {} a -> s {confidence = a} :: Instance)

instance Prelude.FromJSON Instance where
  parseJSON =
    Prelude.withObject
      "Instance"
      ( \x ->
          Instance'
            Prelude.<$> (x Prelude..:? "BoundingBox")
            Prelude.<*> (x Prelude..:? "Confidence")
      )

instance Prelude.Hashable Instance

instance Prelude.NFData Instance
