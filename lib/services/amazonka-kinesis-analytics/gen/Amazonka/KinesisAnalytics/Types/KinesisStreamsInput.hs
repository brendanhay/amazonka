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
-- Module      : Amazonka.KinesisAnalytics.Types.KinesisStreamsInput
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisAnalytics.Types.KinesisStreamsInput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Identifies an Amazon Kinesis stream as the streaming source. You provide
-- the stream\'s Amazon Resource Name (ARN) and an IAM role ARN that
-- enables Amazon Kinesis Analytics to access the stream on your behalf.
--
-- /See:/ 'newKinesisStreamsInput' smart constructor.
data KinesisStreamsInput = KinesisStreamsInput'
  { -- | ARN of the input Amazon Kinesis stream to read.
    resourceARN :: Prelude.Text,
    -- | ARN of the IAM role that Amazon Kinesis Analytics can assume to access
    -- the stream on your behalf. You need to grant the necessary permissions
    -- to this role.
    roleARN :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'KinesisStreamsInput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceARN', 'kinesisStreamsInput_resourceARN' - ARN of the input Amazon Kinesis stream to read.
--
-- 'roleARN', 'kinesisStreamsInput_roleARN' - ARN of the IAM role that Amazon Kinesis Analytics can assume to access
-- the stream on your behalf. You need to grant the necessary permissions
-- to this role.
newKinesisStreamsInput ::
  -- | 'resourceARN'
  Prelude.Text ->
  -- | 'roleARN'
  Prelude.Text ->
  KinesisStreamsInput
newKinesisStreamsInput pResourceARN_ pRoleARN_ =
  KinesisStreamsInput'
    { resourceARN = pResourceARN_,
      roleARN = pRoleARN_
    }

-- | ARN of the input Amazon Kinesis stream to read.
kinesisStreamsInput_resourceARN :: Lens.Lens' KinesisStreamsInput Prelude.Text
kinesisStreamsInput_resourceARN = Lens.lens (\KinesisStreamsInput' {resourceARN} -> resourceARN) (\s@KinesisStreamsInput' {} a -> s {resourceARN = a} :: KinesisStreamsInput)

-- | ARN of the IAM role that Amazon Kinesis Analytics can assume to access
-- the stream on your behalf. You need to grant the necessary permissions
-- to this role.
kinesisStreamsInput_roleARN :: Lens.Lens' KinesisStreamsInput Prelude.Text
kinesisStreamsInput_roleARN = Lens.lens (\KinesisStreamsInput' {roleARN} -> roleARN) (\s@KinesisStreamsInput' {} a -> s {roleARN = a} :: KinesisStreamsInput)

instance Prelude.Hashable KinesisStreamsInput where
  hashWithSalt _salt KinesisStreamsInput' {..} =
    _salt `Prelude.hashWithSalt` resourceARN
      `Prelude.hashWithSalt` roleARN

instance Prelude.NFData KinesisStreamsInput where
  rnf KinesisStreamsInput' {..} =
    Prelude.rnf resourceARN
      `Prelude.seq` Prelude.rnf roleARN

instance Data.ToJSON KinesisStreamsInput where
  toJSON KinesisStreamsInput' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("ResourceARN" Data..= resourceARN),
            Prelude.Just ("RoleARN" Data..= roleARN)
          ]
      )
