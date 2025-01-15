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
-- Module      : Amazonka.QuickSight.Types.DataSetIdentifierDeclaration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.DataSetIdentifierDeclaration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A data set.
--
-- /See:/ 'newDataSetIdentifierDeclaration' smart constructor.
data DataSetIdentifierDeclaration = DataSetIdentifierDeclaration'
  { -- | The identifier of the data set, typically the data set\'s name.
    identifier :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the data set.
    dataSetArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DataSetIdentifierDeclaration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'identifier', 'dataSetIdentifierDeclaration_identifier' - The identifier of the data set, typically the data set\'s name.
--
-- 'dataSetArn', 'dataSetIdentifierDeclaration_dataSetArn' - The Amazon Resource Name (ARN) of the data set.
newDataSetIdentifierDeclaration ::
  -- | 'identifier'
  Prelude.Text ->
  -- | 'dataSetArn'
  Prelude.Text ->
  DataSetIdentifierDeclaration
newDataSetIdentifierDeclaration
  pIdentifier_
  pDataSetArn_ =
    DataSetIdentifierDeclaration'
      { identifier =
          pIdentifier_,
        dataSetArn = pDataSetArn_
      }

-- | The identifier of the data set, typically the data set\'s name.
dataSetIdentifierDeclaration_identifier :: Lens.Lens' DataSetIdentifierDeclaration Prelude.Text
dataSetIdentifierDeclaration_identifier = Lens.lens (\DataSetIdentifierDeclaration' {identifier} -> identifier) (\s@DataSetIdentifierDeclaration' {} a -> s {identifier = a} :: DataSetIdentifierDeclaration)

-- | The Amazon Resource Name (ARN) of the data set.
dataSetIdentifierDeclaration_dataSetArn :: Lens.Lens' DataSetIdentifierDeclaration Prelude.Text
dataSetIdentifierDeclaration_dataSetArn = Lens.lens (\DataSetIdentifierDeclaration' {dataSetArn} -> dataSetArn) (\s@DataSetIdentifierDeclaration' {} a -> s {dataSetArn = a} :: DataSetIdentifierDeclaration)

instance Data.FromJSON DataSetIdentifierDeclaration where
  parseJSON =
    Data.withObject
      "DataSetIdentifierDeclaration"
      ( \x ->
          DataSetIdentifierDeclaration'
            Prelude.<$> (x Data..: "Identifier")
            Prelude.<*> (x Data..: "DataSetArn")
      )

instance
  Prelude.Hashable
    DataSetIdentifierDeclaration
  where
  hashWithSalt _salt DataSetIdentifierDeclaration' {..} =
    _salt
      `Prelude.hashWithSalt` identifier
      `Prelude.hashWithSalt` dataSetArn

instance Prelude.NFData DataSetIdentifierDeclaration where
  rnf DataSetIdentifierDeclaration' {..} =
    Prelude.rnf identifier `Prelude.seq`
      Prelude.rnf dataSetArn

instance Data.ToJSON DataSetIdentifierDeclaration where
  toJSON DataSetIdentifierDeclaration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Identifier" Data..= identifier),
            Prelude.Just ("DataSetArn" Data..= dataSetArn)
          ]
      )
