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
-- Module      : Network.AWS.KinesisAnalytics.Types.KinesisStreamsOutput
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.KinesisStreamsOutput where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | When configuring application output, identifies an Amazon Kinesis stream
-- as the destination. You provide the stream Amazon Resource Name (ARN)
-- and also an IAM role ARN that Amazon Kinesis Analytics can use to write
-- to the stream on your behalf.
--
-- /See:/ 'newKinesisStreamsOutput' smart constructor.
data KinesisStreamsOutput = KinesisStreamsOutput'
  { -- | ARN of the destination Amazon Kinesis stream to write to.
    resourceARN :: Prelude.Text,
    -- | ARN of the IAM role that Amazon Kinesis Analytics can assume to write to
    -- the destination stream on your behalf. You need to grant the necessary
    -- permissions to this role.
    roleARN :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'KinesisStreamsOutput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceARN', 'kinesisStreamsOutput_resourceARN' - ARN of the destination Amazon Kinesis stream to write to.
--
-- 'roleARN', 'kinesisStreamsOutput_roleARN' - ARN of the IAM role that Amazon Kinesis Analytics can assume to write to
-- the destination stream on your behalf. You need to grant the necessary
-- permissions to this role.
newKinesisStreamsOutput ::
  -- | 'resourceARN'
  Prelude.Text ->
  -- | 'roleARN'
  Prelude.Text ->
  KinesisStreamsOutput
newKinesisStreamsOutput pResourceARN_ pRoleARN_ =
  KinesisStreamsOutput'
    { resourceARN = pResourceARN_,
      roleARN = pRoleARN_
    }

-- | ARN of the destination Amazon Kinesis stream to write to.
kinesisStreamsOutput_resourceARN :: Lens.Lens' KinesisStreamsOutput Prelude.Text
kinesisStreamsOutput_resourceARN = Lens.lens (\KinesisStreamsOutput' {resourceARN} -> resourceARN) (\s@KinesisStreamsOutput' {} a -> s {resourceARN = a} :: KinesisStreamsOutput)

-- | ARN of the IAM role that Amazon Kinesis Analytics can assume to write to
-- the destination stream on your behalf. You need to grant the necessary
-- permissions to this role.
kinesisStreamsOutput_roleARN :: Lens.Lens' KinesisStreamsOutput Prelude.Text
kinesisStreamsOutput_roleARN = Lens.lens (\KinesisStreamsOutput' {roleARN} -> roleARN) (\s@KinesisStreamsOutput' {} a -> s {roleARN = a} :: KinesisStreamsOutput)

instance Prelude.Hashable KinesisStreamsOutput

instance Prelude.NFData KinesisStreamsOutput

instance Prelude.ToJSON KinesisStreamsOutput where
  toJSON KinesisStreamsOutput' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("ResourceARN" Prelude..= resourceARN),
            Prelude.Just ("RoleARN" Prelude..= roleARN)
          ]
      )
