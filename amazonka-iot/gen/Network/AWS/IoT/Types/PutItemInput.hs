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
-- Module      : Network.AWS.IoT.Types.PutItemInput
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.PutItemInput where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The input for the DynamoActionVS action that specifies the DynamoDB
-- table to which the message data will be written.
--
-- /See:/ 'newPutItemInput' smart constructor.
data PutItemInput = PutItemInput'
  { -- | The table where the message data will be written.
    tableName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PutItemInput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tableName', 'putItemInput_tableName' - The table where the message data will be written.
newPutItemInput ::
  -- | 'tableName'
  Core.Text ->
  PutItemInput
newPutItemInput pTableName_ =
  PutItemInput' {tableName = pTableName_}

-- | The table where the message data will be written.
putItemInput_tableName :: Lens.Lens' PutItemInput Core.Text
putItemInput_tableName = Lens.lens (\PutItemInput' {tableName} -> tableName) (\s@PutItemInput' {} a -> s {tableName = a} :: PutItemInput)

instance Core.FromJSON PutItemInput where
  parseJSON =
    Core.withObject
      "PutItemInput"
      ( \x ->
          PutItemInput' Core.<$> (x Core..: "tableName")
      )

instance Core.Hashable PutItemInput

instance Core.NFData PutItemInput

instance Core.ToJSON PutItemInput where
  toJSON PutItemInput' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("tableName" Core..= tableName)]
      )
