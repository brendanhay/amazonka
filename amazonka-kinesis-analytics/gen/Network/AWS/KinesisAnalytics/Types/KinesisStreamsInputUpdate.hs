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
-- Module      : Network.AWS.KinesisAnalytics.Types.KinesisStreamsInputUpdate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.KinesisStreamsInputUpdate where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | When updating application input configuration, provides information
-- about an Amazon Kinesis stream as the streaming source.
--
-- /See:/ 'newKinesisStreamsInputUpdate' smart constructor.
data KinesisStreamsInputUpdate = KinesisStreamsInputUpdate'
  { -- | Amazon Resource Name (ARN) of the input Amazon Kinesis stream to read.
    resourceARNUpdate :: Core.Maybe Core.Text,
    -- | ARN of the IAM role that Amazon Kinesis Analytics can assume to access
    -- the stream on your behalf. You need to grant the necessary permissions
    -- to this role.
    roleARNUpdate :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'KinesisStreamsInputUpdate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceARNUpdate', 'kinesisStreamsInputUpdate_resourceARNUpdate' - Amazon Resource Name (ARN) of the input Amazon Kinesis stream to read.
--
-- 'roleARNUpdate', 'kinesisStreamsInputUpdate_roleARNUpdate' - ARN of the IAM role that Amazon Kinesis Analytics can assume to access
-- the stream on your behalf. You need to grant the necessary permissions
-- to this role.
newKinesisStreamsInputUpdate ::
  KinesisStreamsInputUpdate
newKinesisStreamsInputUpdate =
  KinesisStreamsInputUpdate'
    { resourceARNUpdate =
        Core.Nothing,
      roleARNUpdate = Core.Nothing
    }

-- | Amazon Resource Name (ARN) of the input Amazon Kinesis stream to read.
kinesisStreamsInputUpdate_resourceARNUpdate :: Lens.Lens' KinesisStreamsInputUpdate (Core.Maybe Core.Text)
kinesisStreamsInputUpdate_resourceARNUpdate = Lens.lens (\KinesisStreamsInputUpdate' {resourceARNUpdate} -> resourceARNUpdate) (\s@KinesisStreamsInputUpdate' {} a -> s {resourceARNUpdate = a} :: KinesisStreamsInputUpdate)

-- | ARN of the IAM role that Amazon Kinesis Analytics can assume to access
-- the stream on your behalf. You need to grant the necessary permissions
-- to this role.
kinesisStreamsInputUpdate_roleARNUpdate :: Lens.Lens' KinesisStreamsInputUpdate (Core.Maybe Core.Text)
kinesisStreamsInputUpdate_roleARNUpdate = Lens.lens (\KinesisStreamsInputUpdate' {roleARNUpdate} -> roleARNUpdate) (\s@KinesisStreamsInputUpdate' {} a -> s {roleARNUpdate = a} :: KinesisStreamsInputUpdate)

instance Core.Hashable KinesisStreamsInputUpdate

instance Core.NFData KinesisStreamsInputUpdate

instance Core.ToJSON KinesisStreamsInputUpdate where
  toJSON KinesisStreamsInputUpdate' {..} =
    Core.object
      ( Core.catMaybes
          [ ("ResourceARNUpdate" Core..=)
              Core.<$> resourceARNUpdate,
            ("RoleARNUpdate" Core..=) Core.<$> roleARNUpdate
          ]
      )
