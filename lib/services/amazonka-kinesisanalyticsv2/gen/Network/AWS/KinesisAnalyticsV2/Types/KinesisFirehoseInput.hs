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
-- Module      : Network.AWS.KinesisAnalyticsV2.Types.KinesisFirehoseInput
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalyticsV2.Types.KinesisFirehoseInput where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | For a SQL-based Kinesis Data Analytics application, identifies a Kinesis
-- Data Firehose delivery stream as the streaming source. You provide the
-- delivery stream\'s Amazon Resource Name (ARN).
--
-- /See:/ 'newKinesisFirehoseInput' smart constructor.
data KinesisFirehoseInput = KinesisFirehoseInput'
  { -- | The Amazon Resource Name (ARN) of the delivery stream.
    resourceARN :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'KinesisFirehoseInput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceARN', 'kinesisFirehoseInput_resourceARN' - The Amazon Resource Name (ARN) of the delivery stream.
newKinesisFirehoseInput ::
  -- | 'resourceARN'
  Prelude.Text ->
  KinesisFirehoseInput
newKinesisFirehoseInput pResourceARN_ =
  KinesisFirehoseInput' {resourceARN = pResourceARN_}

-- | The Amazon Resource Name (ARN) of the delivery stream.
kinesisFirehoseInput_resourceARN :: Lens.Lens' KinesisFirehoseInput Prelude.Text
kinesisFirehoseInput_resourceARN = Lens.lens (\KinesisFirehoseInput' {resourceARN} -> resourceARN) (\s@KinesisFirehoseInput' {} a -> s {resourceARN = a} :: KinesisFirehoseInput)

instance Prelude.Hashable KinesisFirehoseInput

instance Prelude.NFData KinesisFirehoseInput

instance Core.ToJSON KinesisFirehoseInput where
  toJSON KinesisFirehoseInput' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("ResourceARN" Core..= resourceARN)]
      )
