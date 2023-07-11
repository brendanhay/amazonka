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
-- Module      : Amazonka.KinesisVideo.Types.MappedResourceConfigurationListItem
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisVideo.Types.MappedResourceConfigurationListItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A structure that encapsulates, or contains, the media storage
-- configuration properties.
--
-- /See:/ 'newMappedResourceConfigurationListItem' smart constructor.
data MappedResourceConfigurationListItem = MappedResourceConfigurationListItem'
  { -- | The Amazon Resource Name (ARN) of the Kinesis Video Stream resource,
    -- associated with the stream.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The type of the associated resource for the kinesis video stream.
    type' :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MappedResourceConfigurationListItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'mappedResourceConfigurationListItem_arn' - The Amazon Resource Name (ARN) of the Kinesis Video Stream resource,
-- associated with the stream.
--
-- 'type'', 'mappedResourceConfigurationListItem_type' - The type of the associated resource for the kinesis video stream.
newMappedResourceConfigurationListItem ::
  MappedResourceConfigurationListItem
newMappedResourceConfigurationListItem =
  MappedResourceConfigurationListItem'
    { arn =
        Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the Kinesis Video Stream resource,
-- associated with the stream.
mappedResourceConfigurationListItem_arn :: Lens.Lens' MappedResourceConfigurationListItem (Prelude.Maybe Prelude.Text)
mappedResourceConfigurationListItem_arn = Lens.lens (\MappedResourceConfigurationListItem' {arn} -> arn) (\s@MappedResourceConfigurationListItem' {} a -> s {arn = a} :: MappedResourceConfigurationListItem)

-- | The type of the associated resource for the kinesis video stream.
mappedResourceConfigurationListItem_type :: Lens.Lens' MappedResourceConfigurationListItem (Prelude.Maybe Prelude.Text)
mappedResourceConfigurationListItem_type = Lens.lens (\MappedResourceConfigurationListItem' {type'} -> type') (\s@MappedResourceConfigurationListItem' {} a -> s {type' = a} :: MappedResourceConfigurationListItem)

instance
  Data.FromJSON
    MappedResourceConfigurationListItem
  where
  parseJSON =
    Data.withObject
      "MappedResourceConfigurationListItem"
      ( \x ->
          MappedResourceConfigurationListItem'
            Prelude.<$> (x Data..:? "ARN")
            Prelude.<*> (x Data..:? "Type")
      )

instance
  Prelude.Hashable
    MappedResourceConfigurationListItem
  where
  hashWithSalt
    _salt
    MappedResourceConfigurationListItem' {..} =
      _salt
        `Prelude.hashWithSalt` arn
        `Prelude.hashWithSalt` type'

instance
  Prelude.NFData
    MappedResourceConfigurationListItem
  where
  rnf MappedResourceConfigurationListItem' {..} =
    Prelude.rnf arn `Prelude.seq` Prelude.rnf type'
