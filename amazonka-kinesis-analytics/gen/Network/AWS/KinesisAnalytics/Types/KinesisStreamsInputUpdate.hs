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
-- Module      : Network.AWS.KinesisAnalytics.Types.KinesisStreamsInputUpdate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.KinesisStreamsInputUpdate where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | When updating application input configuration, provides information
-- about an Amazon Kinesis stream as the streaming source.
--
-- /See:/ 'newKinesisStreamsInputUpdate' smart constructor.
data KinesisStreamsInputUpdate = KinesisStreamsInputUpdate'
  { -- | Amazon Resource Name (ARN) of the input Amazon Kinesis stream to read.
    resourceARNUpdate :: Prelude.Maybe Prelude.Text,
    -- | ARN of the IAM role that Amazon Kinesis Analytics can assume to access
    -- the stream on your behalf. You need to grant the necessary permissions
    -- to this role.
    roleARNUpdate :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      roleARNUpdate = Prelude.Nothing
    }

-- | Amazon Resource Name (ARN) of the input Amazon Kinesis stream to read.
kinesisStreamsInputUpdate_resourceARNUpdate :: Lens.Lens' KinesisStreamsInputUpdate (Prelude.Maybe Prelude.Text)
kinesisStreamsInputUpdate_resourceARNUpdate = Lens.lens (\KinesisStreamsInputUpdate' {resourceARNUpdate} -> resourceARNUpdate) (\s@KinesisStreamsInputUpdate' {} a -> s {resourceARNUpdate = a} :: KinesisStreamsInputUpdate)

-- | ARN of the IAM role that Amazon Kinesis Analytics can assume to access
-- the stream on your behalf. You need to grant the necessary permissions
-- to this role.
kinesisStreamsInputUpdate_roleARNUpdate :: Lens.Lens' KinesisStreamsInputUpdate (Prelude.Maybe Prelude.Text)
kinesisStreamsInputUpdate_roleARNUpdate = Lens.lens (\KinesisStreamsInputUpdate' {roleARNUpdate} -> roleARNUpdate) (\s@KinesisStreamsInputUpdate' {} a -> s {roleARNUpdate = a} :: KinesisStreamsInputUpdate)

instance Prelude.Hashable KinesisStreamsInputUpdate

instance Prelude.NFData KinesisStreamsInputUpdate

instance Prelude.ToJSON KinesisStreamsInputUpdate where
  toJSON KinesisStreamsInputUpdate' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("ResourceARNUpdate" Prelude..=)
              Prelude.<$> resourceARNUpdate,
            ("RoleARNUpdate" Prelude..=)
              Prelude.<$> roleARNUpdate
          ]
      )
