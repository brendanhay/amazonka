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
-- Module      : Amazonka.KinesisAnalyticsV2.Types.KinesisStreamsOutputUpdate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisAnalyticsV2.Types.KinesisStreamsOutputUpdate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | When you update a SQL-based Kinesis Data Analytics application\'s output
-- configuration using the UpdateApplication operation, provides
-- information about a Kinesis data stream that is configured as the
-- destination.
--
-- /See:/ 'newKinesisStreamsOutputUpdate' smart constructor.
data KinesisStreamsOutputUpdate = KinesisStreamsOutputUpdate'
  { -- | The Amazon Resource Name (ARN) of the Kinesis data stream where you want
    -- to write the output.
    resourceARNUpdate :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'KinesisStreamsOutputUpdate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceARNUpdate', 'kinesisStreamsOutputUpdate_resourceARNUpdate' - The Amazon Resource Name (ARN) of the Kinesis data stream where you want
-- to write the output.
newKinesisStreamsOutputUpdate ::
  -- | 'resourceARNUpdate'
  Prelude.Text ->
  KinesisStreamsOutputUpdate
newKinesisStreamsOutputUpdate pResourceARNUpdate_ =
  KinesisStreamsOutputUpdate'
    { resourceARNUpdate =
        pResourceARNUpdate_
    }

-- | The Amazon Resource Name (ARN) of the Kinesis data stream where you want
-- to write the output.
kinesisStreamsOutputUpdate_resourceARNUpdate :: Lens.Lens' KinesisStreamsOutputUpdate Prelude.Text
kinesisStreamsOutputUpdate_resourceARNUpdate = Lens.lens (\KinesisStreamsOutputUpdate' {resourceARNUpdate} -> resourceARNUpdate) (\s@KinesisStreamsOutputUpdate' {} a -> s {resourceARNUpdate = a} :: KinesisStreamsOutputUpdate)

instance Prelude.Hashable KinesisStreamsOutputUpdate where
  hashWithSalt _salt KinesisStreamsOutputUpdate' {..} =
    _salt `Prelude.hashWithSalt` resourceARNUpdate

instance Prelude.NFData KinesisStreamsOutputUpdate where
  rnf KinesisStreamsOutputUpdate' {..} =
    Prelude.rnf resourceARNUpdate

instance Data.ToJSON KinesisStreamsOutputUpdate where
  toJSON KinesisStreamsOutputUpdate' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ResourceARNUpdate" Data..= resourceARNUpdate)
          ]
      )
