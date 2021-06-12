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
-- Module      : Network.AWS.Rekognition.Types.HumanLoopDataAttributes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.HumanLoopDataAttributes where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Rekognition.Types.ContentClassifier

-- | Allows you to set attributes of the image. Currently, you can declare an
-- image as free of personally identifiable information.
--
-- /See:/ 'newHumanLoopDataAttributes' smart constructor.
data HumanLoopDataAttributes = HumanLoopDataAttributes'
  { -- | Sets whether the input image is free of personally identifiable
    -- information.
    contentClassifiers :: Core.Maybe [ContentClassifier]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'HumanLoopDataAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'contentClassifiers', 'humanLoopDataAttributes_contentClassifiers' - Sets whether the input image is free of personally identifiable
-- information.
newHumanLoopDataAttributes ::
  HumanLoopDataAttributes
newHumanLoopDataAttributes =
  HumanLoopDataAttributes'
    { contentClassifiers =
        Core.Nothing
    }

-- | Sets whether the input image is free of personally identifiable
-- information.
humanLoopDataAttributes_contentClassifiers :: Lens.Lens' HumanLoopDataAttributes (Core.Maybe [ContentClassifier])
humanLoopDataAttributes_contentClassifiers = Lens.lens (\HumanLoopDataAttributes' {contentClassifiers} -> contentClassifiers) (\s@HumanLoopDataAttributes' {} a -> s {contentClassifiers = a} :: HumanLoopDataAttributes) Core.. Lens.mapping Lens._Coerce

instance Core.Hashable HumanLoopDataAttributes

instance Core.NFData HumanLoopDataAttributes

instance Core.ToJSON HumanLoopDataAttributes where
  toJSON HumanLoopDataAttributes' {..} =
    Core.object
      ( Core.catMaybes
          [ ("ContentClassifiers" Core..=)
              Core.<$> contentClassifiers
          ]
      )
