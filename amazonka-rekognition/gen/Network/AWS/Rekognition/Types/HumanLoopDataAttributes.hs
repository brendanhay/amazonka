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
-- Module      : Network.AWS.Rekognition.Types.HumanLoopDataAttributes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.HumanLoopDataAttributes where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Rekognition.Types.ContentClassifier

-- | Allows you to set attributes of the image. Currently, you can declare an
-- image as free of personally identifiable information.
--
-- /See:/ 'newHumanLoopDataAttributes' smart constructor.
data HumanLoopDataAttributes = HumanLoopDataAttributes'
  { -- | Sets whether the input image is free of personally identifiable
    -- information.
    contentClassifiers :: Prelude.Maybe [ContentClassifier]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing
    }

-- | Sets whether the input image is free of personally identifiable
-- information.
humanLoopDataAttributes_contentClassifiers :: Lens.Lens' HumanLoopDataAttributes (Prelude.Maybe [ContentClassifier])
humanLoopDataAttributes_contentClassifiers = Lens.lens (\HumanLoopDataAttributes' {contentClassifiers} -> contentClassifiers) (\s@HumanLoopDataAttributes' {} a -> s {contentClassifiers = a} :: HumanLoopDataAttributes) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.Hashable HumanLoopDataAttributes

instance Prelude.NFData HumanLoopDataAttributes

instance Prelude.ToJSON HumanLoopDataAttributes where
  toJSON HumanLoopDataAttributes' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("ContentClassifiers" Prelude..=)
              Prelude.<$> contentClassifiers
          ]
      )
