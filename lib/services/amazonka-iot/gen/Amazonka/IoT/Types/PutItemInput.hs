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
-- Module      : Amazonka.IoT.Types.PutItemInput
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.PutItemInput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The input for the DynamoActionVS action that specifies the DynamoDB
-- table to which the message data will be written.
--
-- /See:/ 'newPutItemInput' smart constructor.
data PutItemInput = PutItemInput'
  { -- | The table where the message data will be written.
    tableName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Data.FromJSON PutItemInput where
  parseJSON =
    Data.withObject
      "PutItemInput"
      ( \x ->
          PutItemInput' Prelude.<$> (x Data..: "tableName")
      )

instance Prelude.Hashable PutItemInput where
  hashWithSalt _salt PutItemInput' {..} =
    _salt `Prelude.hashWithSalt` tableName

instance Prelude.NFData PutItemInput where
  rnf PutItemInput' {..} = Prelude.rnf tableName

instance Data.ToJSON PutItemInput where
  toJSON PutItemInput' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("tableName" Data..= tableName)]
      )
