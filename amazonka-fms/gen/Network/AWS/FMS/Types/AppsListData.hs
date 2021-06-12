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
-- Module      : Network.AWS.FMS.Types.AppsListData
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.FMS.Types.AppsListData where

import qualified Network.AWS.Core as Core
import Network.AWS.FMS.Types.App
import qualified Network.AWS.Lens as Lens

-- | An AWS Firewall Manager applications list.
--
-- /See:/ 'newAppsListData' smart constructor.
data AppsListData = AppsListData'
  { -- | The time that the AWS Firewall Manager applications list was last
    -- updated.
    lastUpdateTime :: Core.Maybe Core.POSIX,
    -- | The ID of the AWS Firewall Manager applications list.
    listId :: Core.Maybe Core.Text,
    -- | A map of previous version numbers to their corresponding @App@ object
    -- arrays.
    previousAppsList :: Core.Maybe (Core.HashMap Core.Text [App]),
    -- | The time that the AWS Firewall Manager applications list was created.
    createTime :: Core.Maybe Core.POSIX,
    -- | A unique identifier for each update to the list. When you update the
    -- list, the update token must match the token of the current version of
    -- the application list. You can retrieve the update token by getting the
    -- list.
    listUpdateToken :: Core.Maybe Core.Text,
    -- | The name of the AWS Firewall Manager applications list.
    listName :: Core.Text,
    -- | An array of applications in the AWS Firewall Manager applications list.
    appsList :: [App]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AppsListData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastUpdateTime', 'appsListData_lastUpdateTime' - The time that the AWS Firewall Manager applications list was last
-- updated.
--
-- 'listId', 'appsListData_listId' - The ID of the AWS Firewall Manager applications list.
--
-- 'previousAppsList', 'appsListData_previousAppsList' - A map of previous version numbers to their corresponding @App@ object
-- arrays.
--
-- 'createTime', 'appsListData_createTime' - The time that the AWS Firewall Manager applications list was created.
--
-- 'listUpdateToken', 'appsListData_listUpdateToken' - A unique identifier for each update to the list. When you update the
-- list, the update token must match the token of the current version of
-- the application list. You can retrieve the update token by getting the
-- list.
--
-- 'listName', 'appsListData_listName' - The name of the AWS Firewall Manager applications list.
--
-- 'appsList', 'appsListData_appsList' - An array of applications in the AWS Firewall Manager applications list.
newAppsListData ::
  -- | 'listName'
  Core.Text ->
  AppsListData
newAppsListData pListName_ =
  AppsListData'
    { lastUpdateTime = Core.Nothing,
      listId = Core.Nothing,
      previousAppsList = Core.Nothing,
      createTime = Core.Nothing,
      listUpdateToken = Core.Nothing,
      listName = pListName_,
      appsList = Core.mempty
    }

-- | The time that the AWS Firewall Manager applications list was last
-- updated.
appsListData_lastUpdateTime :: Lens.Lens' AppsListData (Core.Maybe Core.UTCTime)
appsListData_lastUpdateTime = Lens.lens (\AppsListData' {lastUpdateTime} -> lastUpdateTime) (\s@AppsListData' {} a -> s {lastUpdateTime = a} :: AppsListData) Core.. Lens.mapping Core._Time

-- | The ID of the AWS Firewall Manager applications list.
appsListData_listId :: Lens.Lens' AppsListData (Core.Maybe Core.Text)
appsListData_listId = Lens.lens (\AppsListData' {listId} -> listId) (\s@AppsListData' {} a -> s {listId = a} :: AppsListData)

-- | A map of previous version numbers to their corresponding @App@ object
-- arrays.
appsListData_previousAppsList :: Lens.Lens' AppsListData (Core.Maybe (Core.HashMap Core.Text [App]))
appsListData_previousAppsList = Lens.lens (\AppsListData' {previousAppsList} -> previousAppsList) (\s@AppsListData' {} a -> s {previousAppsList = a} :: AppsListData) Core.. Lens.mapping Lens._Coerce

-- | The time that the AWS Firewall Manager applications list was created.
appsListData_createTime :: Lens.Lens' AppsListData (Core.Maybe Core.UTCTime)
appsListData_createTime = Lens.lens (\AppsListData' {createTime} -> createTime) (\s@AppsListData' {} a -> s {createTime = a} :: AppsListData) Core.. Lens.mapping Core._Time

-- | A unique identifier for each update to the list. When you update the
-- list, the update token must match the token of the current version of
-- the application list. You can retrieve the update token by getting the
-- list.
appsListData_listUpdateToken :: Lens.Lens' AppsListData (Core.Maybe Core.Text)
appsListData_listUpdateToken = Lens.lens (\AppsListData' {listUpdateToken} -> listUpdateToken) (\s@AppsListData' {} a -> s {listUpdateToken = a} :: AppsListData)

-- | The name of the AWS Firewall Manager applications list.
appsListData_listName :: Lens.Lens' AppsListData Core.Text
appsListData_listName = Lens.lens (\AppsListData' {listName} -> listName) (\s@AppsListData' {} a -> s {listName = a} :: AppsListData)

-- | An array of applications in the AWS Firewall Manager applications list.
appsListData_appsList :: Lens.Lens' AppsListData [App]
appsListData_appsList = Lens.lens (\AppsListData' {appsList} -> appsList) (\s@AppsListData' {} a -> s {appsList = a} :: AppsListData) Core.. Lens._Coerce

instance Core.FromJSON AppsListData where
  parseJSON =
    Core.withObject
      "AppsListData"
      ( \x ->
          AppsListData'
            Core.<$> (x Core..:? "LastUpdateTime")
            Core.<*> (x Core..:? "ListId")
            Core.<*> (x Core..:? "PreviousAppsList" Core..!= Core.mempty)
            Core.<*> (x Core..:? "CreateTime")
            Core.<*> (x Core..:? "ListUpdateToken")
            Core.<*> (x Core..: "ListName")
            Core.<*> (x Core..:? "AppsList" Core..!= Core.mempty)
      )

instance Core.Hashable AppsListData

instance Core.NFData AppsListData

instance Core.ToJSON AppsListData where
  toJSON AppsListData' {..} =
    Core.object
      ( Core.catMaybes
          [ ("LastUpdateTime" Core..=) Core.<$> lastUpdateTime,
            ("ListId" Core..=) Core.<$> listId,
            ("PreviousAppsList" Core..=)
              Core.<$> previousAppsList,
            ("CreateTime" Core..=) Core.<$> createTime,
            ("ListUpdateToken" Core..=) Core.<$> listUpdateToken,
            Core.Just ("ListName" Core..= listName),
            Core.Just ("AppsList" Core..= appsList)
          ]
      )
