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
-- Module      : Amazonka.SecurityLake.Types.CustomLogSourceAttributes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityLake.Types.CustomLogSourceAttributes where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The attributes of a third-party custom source.
--
-- /See:/ 'newCustomLogSourceAttributes' smart constructor.
data CustomLogSourceAttributes = CustomLogSourceAttributes'
  { -- | The ARN of the Glue crawler.
    crawlerArn :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the Glue database where results are written, such as:
    -- @arn:aws:daylight:us-east-1::database\/sometable\/*@.
    databaseArn :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the Glue table.
    tableArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CustomLogSourceAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'crawlerArn', 'customLogSourceAttributes_crawlerArn' - The ARN of the Glue crawler.
--
-- 'databaseArn', 'customLogSourceAttributes_databaseArn' - The ARN of the Glue database where results are written, such as:
-- @arn:aws:daylight:us-east-1::database\/sometable\/*@.
--
-- 'tableArn', 'customLogSourceAttributes_tableArn' - The ARN of the Glue table.
newCustomLogSourceAttributes ::
  CustomLogSourceAttributes
newCustomLogSourceAttributes =
  CustomLogSourceAttributes'
    { crawlerArn =
        Prelude.Nothing,
      databaseArn = Prelude.Nothing,
      tableArn = Prelude.Nothing
    }

-- | The ARN of the Glue crawler.
customLogSourceAttributes_crawlerArn :: Lens.Lens' CustomLogSourceAttributes (Prelude.Maybe Prelude.Text)
customLogSourceAttributes_crawlerArn = Lens.lens (\CustomLogSourceAttributes' {crawlerArn} -> crawlerArn) (\s@CustomLogSourceAttributes' {} a -> s {crawlerArn = a} :: CustomLogSourceAttributes)

-- | The ARN of the Glue database where results are written, such as:
-- @arn:aws:daylight:us-east-1::database\/sometable\/*@.
customLogSourceAttributes_databaseArn :: Lens.Lens' CustomLogSourceAttributes (Prelude.Maybe Prelude.Text)
customLogSourceAttributes_databaseArn = Lens.lens (\CustomLogSourceAttributes' {databaseArn} -> databaseArn) (\s@CustomLogSourceAttributes' {} a -> s {databaseArn = a} :: CustomLogSourceAttributes)

-- | The ARN of the Glue table.
customLogSourceAttributes_tableArn :: Lens.Lens' CustomLogSourceAttributes (Prelude.Maybe Prelude.Text)
customLogSourceAttributes_tableArn = Lens.lens (\CustomLogSourceAttributes' {tableArn} -> tableArn) (\s@CustomLogSourceAttributes' {} a -> s {tableArn = a} :: CustomLogSourceAttributes)

instance Data.FromJSON CustomLogSourceAttributes where
  parseJSON =
    Data.withObject
      "CustomLogSourceAttributes"
      ( \x ->
          CustomLogSourceAttributes'
            Prelude.<$> (x Data..:? "crawlerArn")
            Prelude.<*> (x Data..:? "databaseArn")
            Prelude.<*> (x Data..:? "tableArn")
      )

instance Prelude.Hashable CustomLogSourceAttributes where
  hashWithSalt _salt CustomLogSourceAttributes' {..} =
    _salt
      `Prelude.hashWithSalt` crawlerArn
      `Prelude.hashWithSalt` databaseArn
      `Prelude.hashWithSalt` tableArn

instance Prelude.NFData CustomLogSourceAttributes where
  rnf CustomLogSourceAttributes' {..} =
    Prelude.rnf crawlerArn
      `Prelude.seq` Prelude.rnf databaseArn
      `Prelude.seq` Prelude.rnf tableArn

instance Data.ToJSON CustomLogSourceAttributes where
  toJSON CustomLogSourceAttributes' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("crawlerArn" Data..=) Prelude.<$> crawlerArn,
            ("databaseArn" Data..=) Prelude.<$> databaseArn,
            ("tableArn" Data..=) Prelude.<$> tableArn
          ]
      )
