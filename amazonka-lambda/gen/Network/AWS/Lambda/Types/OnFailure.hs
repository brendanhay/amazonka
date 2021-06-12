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
-- Module      : Network.AWS.Lambda.Types.OnFailure
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lambda.Types.OnFailure where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A destination for events that failed processing.
--
-- /See:/ 'newOnFailure' smart constructor.
data OnFailure = OnFailure'
  { -- | The Amazon Resource Name (ARN) of the destination resource.
    destination :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'OnFailure' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'destination', 'onFailure_destination' - The Amazon Resource Name (ARN) of the destination resource.
newOnFailure ::
  OnFailure
newOnFailure = OnFailure' {destination = Core.Nothing}

-- | The Amazon Resource Name (ARN) of the destination resource.
onFailure_destination :: Lens.Lens' OnFailure (Core.Maybe Core.Text)
onFailure_destination = Lens.lens (\OnFailure' {destination} -> destination) (\s@OnFailure' {} a -> s {destination = a} :: OnFailure)

instance Core.FromJSON OnFailure where
  parseJSON =
    Core.withObject
      "OnFailure"
      ( \x ->
          OnFailure' Core.<$> (x Core..:? "Destination")
      )

instance Core.Hashable OnFailure

instance Core.NFData OnFailure

instance Core.ToJSON OnFailure where
  toJSON OnFailure' {..} =
    Core.object
      ( Core.catMaybes
          [("Destination" Core..=) Core.<$> destination]
      )
