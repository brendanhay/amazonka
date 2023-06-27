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
-- Module      : Amazonka.Location.Types.MapConfigurationUpdate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Location.Types.MapConfigurationUpdate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies the political view for the style.
--
-- /See:/ 'newMapConfigurationUpdate' smart constructor.
data MapConfigurationUpdate = MapConfigurationUpdate'
  { -- | Specifies the political view for the style. Set to an empty string to
    -- not use a political view, or, for styles that support specific political
    -- views, you can choose a view, such as @IND@ for the Indian view.
    --
    -- Not all map resources or styles support political view styles. See
    -- <https://docs.aws.amazon.com/location/latest/developerguide/map-concepts.html#political-views Political views>
    -- for more information.
    politicalView :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MapConfigurationUpdate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'politicalView', 'mapConfigurationUpdate_politicalView' - Specifies the political view for the style. Set to an empty string to
-- not use a political view, or, for styles that support specific political
-- views, you can choose a view, such as @IND@ for the Indian view.
--
-- Not all map resources or styles support political view styles. See
-- <https://docs.aws.amazon.com/location/latest/developerguide/map-concepts.html#political-views Political views>
-- for more information.
newMapConfigurationUpdate ::
  MapConfigurationUpdate
newMapConfigurationUpdate =
  MapConfigurationUpdate'
    { politicalView =
        Prelude.Nothing
    }

-- | Specifies the political view for the style. Set to an empty string to
-- not use a political view, or, for styles that support specific political
-- views, you can choose a view, such as @IND@ for the Indian view.
--
-- Not all map resources or styles support political view styles. See
-- <https://docs.aws.amazon.com/location/latest/developerguide/map-concepts.html#political-views Political views>
-- for more information.
mapConfigurationUpdate_politicalView :: Lens.Lens' MapConfigurationUpdate (Prelude.Maybe Prelude.Text)
mapConfigurationUpdate_politicalView = Lens.lens (\MapConfigurationUpdate' {politicalView} -> politicalView) (\s@MapConfigurationUpdate' {} a -> s {politicalView = a} :: MapConfigurationUpdate)

instance Prelude.Hashable MapConfigurationUpdate where
  hashWithSalt _salt MapConfigurationUpdate' {..} =
    _salt `Prelude.hashWithSalt` politicalView

instance Prelude.NFData MapConfigurationUpdate where
  rnf MapConfigurationUpdate' {..} =
    Prelude.rnf politicalView

instance Data.ToJSON MapConfigurationUpdate where
  toJSON MapConfigurationUpdate' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("PoliticalView" Data..=)
              Prelude.<$> politicalView
          ]
      )
