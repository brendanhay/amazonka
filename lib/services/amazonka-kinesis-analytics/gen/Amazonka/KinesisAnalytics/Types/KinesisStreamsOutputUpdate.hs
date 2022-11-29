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
-- Module      : Amazonka.KinesisAnalytics.Types.KinesisStreamsOutputUpdate
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisAnalytics.Types.KinesisStreamsOutputUpdate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | When updating an output configuration using the
-- <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_UpdateApplication.html UpdateApplication>
-- operation, provides information about an Amazon Kinesis stream
-- configured as the destination.
--
-- /See:/ 'newKinesisStreamsOutputUpdate' smart constructor.
data KinesisStreamsOutputUpdate = KinesisStreamsOutputUpdate'
  { -- | ARN of the IAM role that Amazon Kinesis Analytics can assume to access
    -- the stream on your behalf. You need to grant the necessary permissions
    -- to this role.
    roleARNUpdate :: Prelude.Maybe Prelude.Text,
    -- | Amazon Resource Name (ARN) of the Amazon Kinesis stream where you want
    -- to write the output.
    resourceARNUpdate :: Prelude.Maybe Prelude.Text
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
-- 'roleARNUpdate', 'kinesisStreamsOutputUpdate_roleARNUpdate' - ARN of the IAM role that Amazon Kinesis Analytics can assume to access
-- the stream on your behalf. You need to grant the necessary permissions
-- to this role.
--
-- 'resourceARNUpdate', 'kinesisStreamsOutputUpdate_resourceARNUpdate' - Amazon Resource Name (ARN) of the Amazon Kinesis stream where you want
-- to write the output.
newKinesisStreamsOutputUpdate ::
  KinesisStreamsOutputUpdate
newKinesisStreamsOutputUpdate =
  KinesisStreamsOutputUpdate'
    { roleARNUpdate =
        Prelude.Nothing,
      resourceARNUpdate = Prelude.Nothing
    }

-- | ARN of the IAM role that Amazon Kinesis Analytics can assume to access
-- the stream on your behalf. You need to grant the necessary permissions
-- to this role.
kinesisStreamsOutputUpdate_roleARNUpdate :: Lens.Lens' KinesisStreamsOutputUpdate (Prelude.Maybe Prelude.Text)
kinesisStreamsOutputUpdate_roleARNUpdate = Lens.lens (\KinesisStreamsOutputUpdate' {roleARNUpdate} -> roleARNUpdate) (\s@KinesisStreamsOutputUpdate' {} a -> s {roleARNUpdate = a} :: KinesisStreamsOutputUpdate)

-- | Amazon Resource Name (ARN) of the Amazon Kinesis stream where you want
-- to write the output.
kinesisStreamsOutputUpdate_resourceARNUpdate :: Lens.Lens' KinesisStreamsOutputUpdate (Prelude.Maybe Prelude.Text)
kinesisStreamsOutputUpdate_resourceARNUpdate = Lens.lens (\KinesisStreamsOutputUpdate' {resourceARNUpdate} -> resourceARNUpdate) (\s@KinesisStreamsOutputUpdate' {} a -> s {resourceARNUpdate = a} :: KinesisStreamsOutputUpdate)

instance Prelude.Hashable KinesisStreamsOutputUpdate where
  hashWithSalt _salt KinesisStreamsOutputUpdate' {..} =
    _salt `Prelude.hashWithSalt` roleARNUpdate
      `Prelude.hashWithSalt` resourceARNUpdate

instance Prelude.NFData KinesisStreamsOutputUpdate where
  rnf KinesisStreamsOutputUpdate' {..} =
    Prelude.rnf roleARNUpdate
      `Prelude.seq` Prelude.rnf resourceARNUpdate

instance Core.ToJSON KinesisStreamsOutputUpdate where
  toJSON KinesisStreamsOutputUpdate' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("RoleARNUpdate" Core..=) Prelude.<$> roleARNUpdate,
            ("ResourceARNUpdate" Core..=)
              Prelude.<$> resourceARNUpdate
          ]
      )
