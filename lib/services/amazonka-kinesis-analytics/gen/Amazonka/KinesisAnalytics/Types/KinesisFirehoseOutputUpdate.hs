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
-- Module      : Amazonka.KinesisAnalytics.Types.KinesisFirehoseOutputUpdate
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisAnalytics.Types.KinesisFirehoseOutputUpdate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | When updating an output configuration using the
-- <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_UpdateApplication.html UpdateApplication>
-- operation, provides information about an Amazon Kinesis Firehose
-- delivery stream configured as the destination.
--
-- /See:/ 'newKinesisFirehoseOutputUpdate' smart constructor.
data KinesisFirehoseOutputUpdate = KinesisFirehoseOutputUpdate'
  { -- | ARN of the IAM role that Amazon Kinesis Analytics can assume to access
    -- the stream on your behalf. You need to grant the necessary permissions
    -- to this role.
    roleARNUpdate :: Prelude.Maybe Prelude.Text,
    -- | Amazon Resource Name (ARN) of the Amazon Kinesis Firehose delivery
    -- stream to write to.
    resourceARNUpdate :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'KinesisFirehoseOutputUpdate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roleARNUpdate', 'kinesisFirehoseOutputUpdate_roleARNUpdate' - ARN of the IAM role that Amazon Kinesis Analytics can assume to access
-- the stream on your behalf. You need to grant the necessary permissions
-- to this role.
--
-- 'resourceARNUpdate', 'kinesisFirehoseOutputUpdate_resourceARNUpdate' - Amazon Resource Name (ARN) of the Amazon Kinesis Firehose delivery
-- stream to write to.
newKinesisFirehoseOutputUpdate ::
  KinesisFirehoseOutputUpdate
newKinesisFirehoseOutputUpdate =
  KinesisFirehoseOutputUpdate'
    { roleARNUpdate =
        Prelude.Nothing,
      resourceARNUpdate = Prelude.Nothing
    }

-- | ARN of the IAM role that Amazon Kinesis Analytics can assume to access
-- the stream on your behalf. You need to grant the necessary permissions
-- to this role.
kinesisFirehoseOutputUpdate_roleARNUpdate :: Lens.Lens' KinesisFirehoseOutputUpdate (Prelude.Maybe Prelude.Text)
kinesisFirehoseOutputUpdate_roleARNUpdate = Lens.lens (\KinesisFirehoseOutputUpdate' {roleARNUpdate} -> roleARNUpdate) (\s@KinesisFirehoseOutputUpdate' {} a -> s {roleARNUpdate = a} :: KinesisFirehoseOutputUpdate)

-- | Amazon Resource Name (ARN) of the Amazon Kinesis Firehose delivery
-- stream to write to.
kinesisFirehoseOutputUpdate_resourceARNUpdate :: Lens.Lens' KinesisFirehoseOutputUpdate (Prelude.Maybe Prelude.Text)
kinesisFirehoseOutputUpdate_resourceARNUpdate = Lens.lens (\KinesisFirehoseOutputUpdate' {resourceARNUpdate} -> resourceARNUpdate) (\s@KinesisFirehoseOutputUpdate' {} a -> s {resourceARNUpdate = a} :: KinesisFirehoseOutputUpdate)

instance Prelude.Hashable KinesisFirehoseOutputUpdate where
  hashWithSalt _salt KinesisFirehoseOutputUpdate' {..} =
    _salt `Prelude.hashWithSalt` roleARNUpdate
      `Prelude.hashWithSalt` resourceARNUpdate

instance Prelude.NFData KinesisFirehoseOutputUpdate where
  rnf KinesisFirehoseOutputUpdate' {..} =
    Prelude.rnf roleARNUpdate
      `Prelude.seq` Prelude.rnf resourceARNUpdate

instance Core.ToJSON KinesisFirehoseOutputUpdate where
  toJSON KinesisFirehoseOutputUpdate' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("RoleARNUpdate" Core..=) Prelude.<$> roleARNUpdate,
            ("ResourceARNUpdate" Core..=)
              Prelude.<$> resourceARNUpdate
          ]
      )
