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
-- Module      : Amazonka.KinesisAnalyticsV2.Types.KinesisFirehoseInputUpdate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisAnalyticsV2.Types.KinesisFirehoseInputUpdate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | For a SQL-based Kinesis Data Analytics application, when updating
-- application input configuration, provides information about a Kinesis
-- Data Firehose delivery stream as the streaming source.
--
-- /See:/ 'newKinesisFirehoseInputUpdate' smart constructor.
data KinesisFirehoseInputUpdate = KinesisFirehoseInputUpdate'
  { -- | The Amazon Resource Name (ARN) of the input delivery stream to read.
    resourceARNUpdate :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'KinesisFirehoseInputUpdate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceARNUpdate', 'kinesisFirehoseInputUpdate_resourceARNUpdate' - The Amazon Resource Name (ARN) of the input delivery stream to read.
newKinesisFirehoseInputUpdate ::
  -- | 'resourceARNUpdate'
  Prelude.Text ->
  KinesisFirehoseInputUpdate
newKinesisFirehoseInputUpdate pResourceARNUpdate_ =
  KinesisFirehoseInputUpdate'
    { resourceARNUpdate =
        pResourceARNUpdate_
    }

-- | The Amazon Resource Name (ARN) of the input delivery stream to read.
kinesisFirehoseInputUpdate_resourceARNUpdate :: Lens.Lens' KinesisFirehoseInputUpdate Prelude.Text
kinesisFirehoseInputUpdate_resourceARNUpdate = Lens.lens (\KinesisFirehoseInputUpdate' {resourceARNUpdate} -> resourceARNUpdate) (\s@KinesisFirehoseInputUpdate' {} a -> s {resourceARNUpdate = a} :: KinesisFirehoseInputUpdate)

instance Prelude.Hashable KinesisFirehoseInputUpdate where
  hashWithSalt _salt KinesisFirehoseInputUpdate' {..} =
    _salt `Prelude.hashWithSalt` resourceARNUpdate

instance Prelude.NFData KinesisFirehoseInputUpdate where
  rnf KinesisFirehoseInputUpdate' {..} =
    Prelude.rnf resourceARNUpdate

instance Data.ToJSON KinesisFirehoseInputUpdate where
  toJSON KinesisFirehoseInputUpdate' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ResourceARNUpdate" Data..= resourceARNUpdate)
          ]
      )
