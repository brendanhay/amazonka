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
-- Module      : Network.AWS.SSM.Types.MetadataValue
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.MetadataValue where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Metadata to assign to an Application Manager application.
--
-- /See:/ 'newMetadataValue' smart constructor.
data MetadataValue = MetadataValue'
  { -- | Metadata value to assign to an Application Manager application.
    value :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MetadataValue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'value', 'metadataValue_value' - Metadata value to assign to an Application Manager application.
newMetadataValue ::
  MetadataValue
newMetadataValue =
  MetadataValue' {value = Prelude.Nothing}

-- | Metadata value to assign to an Application Manager application.
metadataValue_value :: Lens.Lens' MetadataValue (Prelude.Maybe Prelude.Text)
metadataValue_value = Lens.lens (\MetadataValue' {value} -> value) (\s@MetadataValue' {} a -> s {value = a} :: MetadataValue)

instance Core.FromJSON MetadataValue where
  parseJSON =
    Core.withObject
      "MetadataValue"
      ( \x ->
          MetadataValue' Prelude.<$> (x Core..:? "Value")
      )

instance Prelude.Hashable MetadataValue

instance Prelude.NFData MetadataValue

instance Core.ToJSON MetadataValue where
  toJSON MetadataValue' {..} =
    Core.object
      ( Prelude.catMaybes
          [("Value" Core..=) Prelude.<$> value]
      )
