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
-- Module      : Network.AWS.KinesisAnalytics.Types.KinesisStreamsOutputUpdate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.KinesisStreamsOutputUpdate where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | When updating an output configuration using the
-- <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_UpdateApplication.html UpdateApplication>
-- operation, provides information about an Amazon Kinesis stream
-- configured as the destination.
--
-- /See:/ 'newKinesisStreamsOutputUpdate' smart constructor.
data KinesisStreamsOutputUpdate = KinesisStreamsOutputUpdate'
  { -- | Amazon Resource Name (ARN) of the Amazon Kinesis stream where you want
    -- to write the output.
    resourceARNUpdate :: Prelude.Maybe Prelude.Text,
    -- | ARN of the IAM role that Amazon Kinesis Analytics can assume to access
    -- the stream on your behalf. You need to grant the necessary permissions
    -- to this role.
    roleARNUpdate :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'KinesisStreamsOutputUpdate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceARNUpdate', 'kinesisStreamsOutputUpdate_resourceARNUpdate' - Amazon Resource Name (ARN) of the Amazon Kinesis stream where you want
-- to write the output.
--
-- 'roleARNUpdate', 'kinesisStreamsOutputUpdate_roleARNUpdate' - ARN of the IAM role that Amazon Kinesis Analytics can assume to access
-- the stream on your behalf. You need to grant the necessary permissions
-- to this role.
newKinesisStreamsOutputUpdate ::
  KinesisStreamsOutputUpdate
newKinesisStreamsOutputUpdate =
  KinesisStreamsOutputUpdate'
    { resourceARNUpdate =
        Prelude.Nothing,
      roleARNUpdate = Prelude.Nothing
    }

-- | Amazon Resource Name (ARN) of the Amazon Kinesis stream where you want
-- to write the output.
kinesisStreamsOutputUpdate_resourceARNUpdate :: Lens.Lens' KinesisStreamsOutputUpdate (Prelude.Maybe Prelude.Text)
kinesisStreamsOutputUpdate_resourceARNUpdate = Lens.lens (\KinesisStreamsOutputUpdate' {resourceARNUpdate} -> resourceARNUpdate) (\s@KinesisStreamsOutputUpdate' {} a -> s {resourceARNUpdate = a} :: KinesisStreamsOutputUpdate)

-- | ARN of the IAM role that Amazon Kinesis Analytics can assume to access
-- the stream on your behalf. You need to grant the necessary permissions
-- to this role.
kinesisStreamsOutputUpdate_roleARNUpdate :: Lens.Lens' KinesisStreamsOutputUpdate (Prelude.Maybe Prelude.Text)
kinesisStreamsOutputUpdate_roleARNUpdate = Lens.lens (\KinesisStreamsOutputUpdate' {roleARNUpdate} -> roleARNUpdate) (\s@KinesisStreamsOutputUpdate' {} a -> s {roleARNUpdate = a} :: KinesisStreamsOutputUpdate)

instance Prelude.Hashable KinesisStreamsOutputUpdate

instance Prelude.NFData KinesisStreamsOutputUpdate

instance Prelude.ToJSON KinesisStreamsOutputUpdate where
  toJSON KinesisStreamsOutputUpdate' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("ResourceARNUpdate" Prelude..=)
              Prelude.<$> resourceARNUpdate,
            ("RoleARNUpdate" Prelude..=)
              Prelude.<$> roleARNUpdate
          ]
      )
