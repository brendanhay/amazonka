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
-- Module      : Network.AWS.IoT.Types.PutItemInput
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.PutItemInput where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The input for the DynamoActionVS action that specifies the DynamoDB
-- table to which the message data will be written.
--
-- /See:/ 'newPutItemInput' smart constructor.
data PutItemInput = PutItemInput'
  { -- | The table where the message data will be written.
    tableName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  PutItemInput
newPutItemInput pTableName_ =
  PutItemInput' {tableName = pTableName_}

-- | The table where the message data will be written.
putItemInput_tableName :: Lens.Lens' PutItemInput Prelude.Text
putItemInput_tableName = Lens.lens (\PutItemInput' {tableName} -> tableName) (\s@PutItemInput' {} a -> s {tableName = a} :: PutItemInput)

instance Prelude.FromJSON PutItemInput where
  parseJSON =
    Prelude.withObject
      "PutItemInput"
      ( \x ->
          PutItemInput' Prelude.<$> (x Prelude..: "tableName")
      )

instance Prelude.Hashable PutItemInput

instance Prelude.NFData PutItemInput

instance Prelude.ToJSON PutItemInput where
  toJSON PutItemInput' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("tableName" Prelude..= tableName)]
      )
