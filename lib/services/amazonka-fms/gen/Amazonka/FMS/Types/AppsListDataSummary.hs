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
-- Module      : Amazonka.FMS.Types.AppsListDataSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FMS.Types.AppsListDataSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.FMS.Types.App
import qualified Amazonka.Prelude as Prelude

-- | Details of the Firewall Manager applications list.
--
-- /See:/ 'newAppsListDataSummary' smart constructor.
data AppsListDataSummary = AppsListDataSummary'
  { -- | The ID of the applications list.
    listId :: Prelude.Maybe Prelude.Text,
    -- | An array of @App@ objects in the Firewall Manager applications list.
    appsList :: Prelude.Maybe [App],
    -- | The name of the applications list.
    listName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the applications list.
    listArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AppsListDataSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'listId', 'appsListDataSummary_listId' - The ID of the applications list.
--
-- 'appsList', 'appsListDataSummary_appsList' - An array of @App@ objects in the Firewall Manager applications list.
--
-- 'listName', 'appsListDataSummary_listName' - The name of the applications list.
--
-- 'listArn', 'appsListDataSummary_listArn' - The Amazon Resource Name (ARN) of the applications list.
newAppsListDataSummary ::
  AppsListDataSummary
newAppsListDataSummary =
  AppsListDataSummary'
    { listId = Prelude.Nothing,
      appsList = Prelude.Nothing,
      listName = Prelude.Nothing,
      listArn = Prelude.Nothing
    }

-- | The ID of the applications list.
appsListDataSummary_listId :: Lens.Lens' AppsListDataSummary (Prelude.Maybe Prelude.Text)
appsListDataSummary_listId = Lens.lens (\AppsListDataSummary' {listId} -> listId) (\s@AppsListDataSummary' {} a -> s {listId = a} :: AppsListDataSummary)

-- | An array of @App@ objects in the Firewall Manager applications list.
appsListDataSummary_appsList :: Lens.Lens' AppsListDataSummary (Prelude.Maybe [App])
appsListDataSummary_appsList = Lens.lens (\AppsListDataSummary' {appsList} -> appsList) (\s@AppsListDataSummary' {} a -> s {appsList = a} :: AppsListDataSummary) Prelude.. Lens.mapping Lens.coerced

-- | The name of the applications list.
appsListDataSummary_listName :: Lens.Lens' AppsListDataSummary (Prelude.Maybe Prelude.Text)
appsListDataSummary_listName = Lens.lens (\AppsListDataSummary' {listName} -> listName) (\s@AppsListDataSummary' {} a -> s {listName = a} :: AppsListDataSummary)

-- | The Amazon Resource Name (ARN) of the applications list.
appsListDataSummary_listArn :: Lens.Lens' AppsListDataSummary (Prelude.Maybe Prelude.Text)
appsListDataSummary_listArn = Lens.lens (\AppsListDataSummary' {listArn} -> listArn) (\s@AppsListDataSummary' {} a -> s {listArn = a} :: AppsListDataSummary)

instance Core.FromJSON AppsListDataSummary where
  parseJSON =
    Core.withObject
      "AppsListDataSummary"
      ( \x ->
          AppsListDataSummary'
            Prelude.<$> (x Core..:? "ListId")
            Prelude.<*> (x Core..:? "AppsList" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "ListName")
            Prelude.<*> (x Core..:? "ListArn")
      )

instance Prelude.Hashable AppsListDataSummary where
  hashWithSalt _salt AppsListDataSummary' {..} =
    _salt `Prelude.hashWithSalt` listId
      `Prelude.hashWithSalt` appsList
      `Prelude.hashWithSalt` listName
      `Prelude.hashWithSalt` listArn

instance Prelude.NFData AppsListDataSummary where
  rnf AppsListDataSummary' {..} =
    Prelude.rnf listId
      `Prelude.seq` Prelude.rnf appsList
      `Prelude.seq` Prelude.rnf listName
      `Prelude.seq` Prelude.rnf listArn
