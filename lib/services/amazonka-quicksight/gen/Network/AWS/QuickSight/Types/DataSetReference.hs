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
-- Module      : Network.AWS.QuickSight.Types.DataSetReference
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.QuickSight.Types.DataSetReference where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Dataset reference.
--
-- /See:/ 'newDataSetReference' smart constructor.
data DataSetReference = DataSetReference'
  { -- | Dataset placeholder.
    dataSetPlaceholder :: Prelude.Text,
    -- | Dataset Amazon Resource Name (ARN).
    dataSetArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DataSetReference' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataSetPlaceholder', 'dataSetReference_dataSetPlaceholder' - Dataset placeholder.
--
-- 'dataSetArn', 'dataSetReference_dataSetArn' - Dataset Amazon Resource Name (ARN).
newDataSetReference ::
  -- | 'dataSetPlaceholder'
  Prelude.Text ->
  -- | 'dataSetArn'
  Prelude.Text ->
  DataSetReference
newDataSetReference pDataSetPlaceholder_ pDataSetArn_ =
  DataSetReference'
    { dataSetPlaceholder =
        pDataSetPlaceholder_,
      dataSetArn = pDataSetArn_
    }

-- | Dataset placeholder.
dataSetReference_dataSetPlaceholder :: Lens.Lens' DataSetReference Prelude.Text
dataSetReference_dataSetPlaceholder = Lens.lens (\DataSetReference' {dataSetPlaceholder} -> dataSetPlaceholder) (\s@DataSetReference' {} a -> s {dataSetPlaceholder = a} :: DataSetReference)

-- | Dataset Amazon Resource Name (ARN).
dataSetReference_dataSetArn :: Lens.Lens' DataSetReference Prelude.Text
dataSetReference_dataSetArn = Lens.lens (\DataSetReference' {dataSetArn} -> dataSetArn) (\s@DataSetReference' {} a -> s {dataSetArn = a} :: DataSetReference)

instance Prelude.Hashable DataSetReference

instance Prelude.NFData DataSetReference

instance Core.ToJSON DataSetReference where
  toJSON DataSetReference' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("DataSetPlaceholder" Core..= dataSetPlaceholder),
            Prelude.Just ("DataSetArn" Core..= dataSetArn)
          ]
      )
