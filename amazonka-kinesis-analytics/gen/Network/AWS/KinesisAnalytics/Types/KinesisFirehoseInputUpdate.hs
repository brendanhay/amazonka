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
-- Module      : Network.AWS.KinesisAnalytics.Types.KinesisFirehoseInputUpdate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.KinesisFirehoseInputUpdate where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | When updating application input configuration, provides information
-- about an Amazon Kinesis Firehose delivery stream as the streaming
-- source.
--
-- /See:/ 'newKinesisFirehoseInputUpdate' smart constructor.
data KinesisFirehoseInputUpdate = KinesisFirehoseInputUpdate'
  { -- | Amazon Resource Name (ARN) of the input Amazon Kinesis Firehose delivery
    -- stream to read.
    resourceARNUpdate :: Prelude.Maybe Prelude.Text,
    -- | ARN of the IAM role that Amazon Kinesis Analytics can assume to access
    -- the stream on your behalf. You need to grant the necessary permissions
    -- to this role.
    roleARNUpdate :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'KinesisFirehoseInputUpdate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceARNUpdate', 'kinesisFirehoseInputUpdate_resourceARNUpdate' - Amazon Resource Name (ARN) of the input Amazon Kinesis Firehose delivery
-- stream to read.
--
-- 'roleARNUpdate', 'kinesisFirehoseInputUpdate_roleARNUpdate' - ARN of the IAM role that Amazon Kinesis Analytics can assume to access
-- the stream on your behalf. You need to grant the necessary permissions
-- to this role.
newKinesisFirehoseInputUpdate ::
  KinesisFirehoseInputUpdate
newKinesisFirehoseInputUpdate =
  KinesisFirehoseInputUpdate'
    { resourceARNUpdate =
        Prelude.Nothing,
      roleARNUpdate = Prelude.Nothing
    }

-- | Amazon Resource Name (ARN) of the input Amazon Kinesis Firehose delivery
-- stream to read.
kinesisFirehoseInputUpdate_resourceARNUpdate :: Lens.Lens' KinesisFirehoseInputUpdate (Prelude.Maybe Prelude.Text)
kinesisFirehoseInputUpdate_resourceARNUpdate = Lens.lens (\KinesisFirehoseInputUpdate' {resourceARNUpdate} -> resourceARNUpdate) (\s@KinesisFirehoseInputUpdate' {} a -> s {resourceARNUpdate = a} :: KinesisFirehoseInputUpdate)

-- | ARN of the IAM role that Amazon Kinesis Analytics can assume to access
-- the stream on your behalf. You need to grant the necessary permissions
-- to this role.
kinesisFirehoseInputUpdate_roleARNUpdate :: Lens.Lens' KinesisFirehoseInputUpdate (Prelude.Maybe Prelude.Text)
kinesisFirehoseInputUpdate_roleARNUpdate = Lens.lens (\KinesisFirehoseInputUpdate' {roleARNUpdate} -> roleARNUpdate) (\s@KinesisFirehoseInputUpdate' {} a -> s {roleARNUpdate = a} :: KinesisFirehoseInputUpdate)

instance Prelude.Hashable KinesisFirehoseInputUpdate

instance Prelude.NFData KinesisFirehoseInputUpdate

instance Prelude.ToJSON KinesisFirehoseInputUpdate where
  toJSON KinesisFirehoseInputUpdate' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("ResourceARNUpdate" Prelude..=)
              Prelude.<$> resourceARNUpdate,
            ("RoleARNUpdate" Prelude..=)
              Prelude.<$> roleARNUpdate
          ]
      )
