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
-- Module      : Amazonka.KinesisAnalyticsV2.Types.KinesisFirehoseOutput
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisAnalyticsV2.Types.KinesisFirehoseOutput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | For a SQL-based Kinesis Data Analytics application, when configuring
-- application output, identifies a Kinesis Data Firehose delivery stream
-- as the destination. You provide the stream Amazon Resource Name (ARN) of
-- the delivery stream.
--
-- /See:/ 'newKinesisFirehoseOutput' smart constructor.
data KinesisFirehoseOutput = KinesisFirehoseOutput'
  { -- | The ARN of the destination delivery stream to write to.
    resourceARN :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'KinesisFirehoseOutput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceARN', 'kinesisFirehoseOutput_resourceARN' - The ARN of the destination delivery stream to write to.
newKinesisFirehoseOutput ::
  -- | 'resourceARN'
  Prelude.Text ->
  KinesisFirehoseOutput
newKinesisFirehoseOutput pResourceARN_ =
  KinesisFirehoseOutput' {resourceARN = pResourceARN_}

-- | The ARN of the destination delivery stream to write to.
kinesisFirehoseOutput_resourceARN :: Lens.Lens' KinesisFirehoseOutput Prelude.Text
kinesisFirehoseOutput_resourceARN = Lens.lens (\KinesisFirehoseOutput' {resourceARN} -> resourceARN) (\s@KinesisFirehoseOutput' {} a -> s {resourceARN = a} :: KinesisFirehoseOutput)

instance Prelude.Hashable KinesisFirehoseOutput where
  hashWithSalt _salt KinesisFirehoseOutput' {..} =
    _salt `Prelude.hashWithSalt` resourceARN

instance Prelude.NFData KinesisFirehoseOutput where
  rnf KinesisFirehoseOutput' {..} =
    Prelude.rnf resourceARN

instance Data.ToJSON KinesisFirehoseOutput where
  toJSON KinesisFirehoseOutput' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("ResourceARN" Data..= resourceARN)]
      )
