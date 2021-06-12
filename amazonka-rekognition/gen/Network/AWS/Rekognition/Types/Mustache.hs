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
-- Module      : Network.AWS.Rekognition.Types.Mustache
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.Mustache where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Indicates whether or not the face has a mustache, and the confidence
-- level in the determination.
--
-- /See:/ 'newMustache' smart constructor.
data Mustache = Mustache'
  { -- | Level of confidence in the determination.
    confidence :: Core.Maybe Core.Double,
    -- | Boolean value that indicates whether the face has mustache or not.
    value :: Core.Maybe Core.Bool
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Mustache' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'confidence', 'mustache_confidence' - Level of confidence in the determination.
--
-- 'value', 'mustache_value' - Boolean value that indicates whether the face has mustache or not.
newMustache ::
  Mustache
newMustache =
  Mustache'
    { confidence = Core.Nothing,
      value = Core.Nothing
    }

-- | Level of confidence in the determination.
mustache_confidence :: Lens.Lens' Mustache (Core.Maybe Core.Double)
mustache_confidence = Lens.lens (\Mustache' {confidence} -> confidence) (\s@Mustache' {} a -> s {confidence = a} :: Mustache)

-- | Boolean value that indicates whether the face has mustache or not.
mustache_value :: Lens.Lens' Mustache (Core.Maybe Core.Bool)
mustache_value = Lens.lens (\Mustache' {value} -> value) (\s@Mustache' {} a -> s {value = a} :: Mustache)

instance Core.FromJSON Mustache where
  parseJSON =
    Core.withObject
      "Mustache"
      ( \x ->
          Mustache'
            Core.<$> (x Core..:? "Confidence")
            Core.<*> (x Core..:? "Value")
      )

instance Core.Hashable Mustache

instance Core.NFData Mustache
