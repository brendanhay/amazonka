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
-- Module      : Amazonka.QuickSight.Types.EmptyVisual
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.EmptyVisual where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.VisualCustomAction

-- | An empty visual.
--
-- Empty visuals are used in layouts but have not been configured to show
-- any data. A new visual created in the Amazon QuickSight console is
-- considered an @EmptyVisual@ until a visual type is selected.
--
-- /See:/ 'newEmptyVisual' smart constructor.
data EmptyVisual = EmptyVisual'
  { -- | The list of custom actions that are configured for a visual.
    actions :: Prelude.Maybe [VisualCustomAction],
    -- | The unique identifier of a visual. This identifier must be unique within
    -- the context of a dashboard, template, or analysis. Two dashboards,
    -- analyses, or templates can have visuals with the same identifiers.
    visualId :: Prelude.Text,
    -- | The data set that is used in the empty visual. Every visual requires a
    -- dataset to render.
    dataSetIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EmptyVisual' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'actions', 'emptyVisual_actions' - The list of custom actions that are configured for a visual.
--
-- 'visualId', 'emptyVisual_visualId' - The unique identifier of a visual. This identifier must be unique within
-- the context of a dashboard, template, or analysis. Two dashboards,
-- analyses, or templates can have visuals with the same identifiers.
--
-- 'dataSetIdentifier', 'emptyVisual_dataSetIdentifier' - The data set that is used in the empty visual. Every visual requires a
-- dataset to render.
newEmptyVisual ::
  -- | 'visualId'
  Prelude.Text ->
  -- | 'dataSetIdentifier'
  Prelude.Text ->
  EmptyVisual
newEmptyVisual pVisualId_ pDataSetIdentifier_ =
  EmptyVisual'
    { actions = Prelude.Nothing,
      visualId = pVisualId_,
      dataSetIdentifier = pDataSetIdentifier_
    }

-- | The list of custom actions that are configured for a visual.
emptyVisual_actions :: Lens.Lens' EmptyVisual (Prelude.Maybe [VisualCustomAction])
emptyVisual_actions = Lens.lens (\EmptyVisual' {actions} -> actions) (\s@EmptyVisual' {} a -> s {actions = a} :: EmptyVisual) Prelude.. Lens.mapping Lens.coerced

-- | The unique identifier of a visual. This identifier must be unique within
-- the context of a dashboard, template, or analysis. Two dashboards,
-- analyses, or templates can have visuals with the same identifiers.
emptyVisual_visualId :: Lens.Lens' EmptyVisual Prelude.Text
emptyVisual_visualId = Lens.lens (\EmptyVisual' {visualId} -> visualId) (\s@EmptyVisual' {} a -> s {visualId = a} :: EmptyVisual)

-- | The data set that is used in the empty visual. Every visual requires a
-- dataset to render.
emptyVisual_dataSetIdentifier :: Lens.Lens' EmptyVisual Prelude.Text
emptyVisual_dataSetIdentifier = Lens.lens (\EmptyVisual' {dataSetIdentifier} -> dataSetIdentifier) (\s@EmptyVisual' {} a -> s {dataSetIdentifier = a} :: EmptyVisual)

instance Data.FromJSON EmptyVisual where
  parseJSON =
    Data.withObject
      "EmptyVisual"
      ( \x ->
          EmptyVisual'
            Prelude.<$> (x Data..:? "Actions" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "VisualId")
            Prelude.<*> (x Data..: "DataSetIdentifier")
      )

instance Prelude.Hashable EmptyVisual where
  hashWithSalt _salt EmptyVisual' {..} =
    _salt
      `Prelude.hashWithSalt` actions
      `Prelude.hashWithSalt` visualId
      `Prelude.hashWithSalt` dataSetIdentifier

instance Prelude.NFData EmptyVisual where
  rnf EmptyVisual' {..} =
    Prelude.rnf actions
      `Prelude.seq` Prelude.rnf visualId
      `Prelude.seq` Prelude.rnf dataSetIdentifier

instance Data.ToJSON EmptyVisual where
  toJSON EmptyVisual' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Actions" Data..=) Prelude.<$> actions,
            Prelude.Just ("VisualId" Data..= visualId),
            Prelude.Just
              ("DataSetIdentifier" Data..= dataSetIdentifier)
          ]
      )
