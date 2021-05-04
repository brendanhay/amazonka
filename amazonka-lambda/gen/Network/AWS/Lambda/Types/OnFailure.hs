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
-- Module      : Network.AWS.Lambda.Types.OnFailure
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lambda.Types.OnFailure where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A destination for events that failed processing.
--
-- /See:/ 'newOnFailure' smart constructor.
data OnFailure = OnFailure'
  { -- | The Amazon Resource Name (ARN) of the destination resource.
    destination :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
newOnFailure =
  OnFailure' {destination = Prelude.Nothing}

-- | The Amazon Resource Name (ARN) of the destination resource.
onFailure_destination :: Lens.Lens' OnFailure (Prelude.Maybe Prelude.Text)
onFailure_destination = Lens.lens (\OnFailure' {destination} -> destination) (\s@OnFailure' {} a -> s {destination = a} :: OnFailure)

instance Prelude.FromJSON OnFailure where
  parseJSON =
    Prelude.withObject
      "OnFailure"
      ( \x ->
          OnFailure' Prelude.<$> (x Prelude..:? "Destination")
      )

instance Prelude.Hashable OnFailure

instance Prelude.NFData OnFailure

instance Prelude.ToJSON OnFailure where
  toJSON OnFailure' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [("Destination" Prelude..=) Prelude.<$> destination]
      )
