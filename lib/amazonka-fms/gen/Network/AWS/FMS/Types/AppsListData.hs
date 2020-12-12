{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FMS.Types.AppsListData
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.FMS.Types.AppsListData
  ( AppsListData (..),

    -- * Smart constructor
    mkAppsListData,

    -- * Lenses
    aldListUpdateToken,
    aldListId,
    aldLastUpdateTime,
    aldPreviousAppsList,
    aldCreateTime,
    aldListName,
    aldAppsList,
  )
where

import Network.AWS.FMS.Types.App
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An AWS Firewall Manager applications list.
--
-- /See:/ 'mkAppsListData' smart constructor.
data AppsListData = AppsListData'
  { listUpdateToken ::
      Lude.Maybe Lude.Text,
    listId :: Lude.Maybe Lude.Text,
    lastUpdateTime :: Lude.Maybe Lude.Timestamp,
    previousAppsList :: Lude.Maybe (Lude.HashMap Lude.Text ([App])),
    createTime :: Lude.Maybe Lude.Timestamp,
    listName :: Lude.Text,
    appsList :: [App]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AppsListData' with the minimum fields required to make a request.
--
-- * 'appsList' - An array of applications in the AWS Firewall Manager applications list.
-- * 'createTime' - The time that the AWS Firewall Manager applications list was created.
-- * 'lastUpdateTime' - The time that the AWS Firewall Manager applications list was last updated.
-- * 'listId' - The ID of the AWS Firewall Manager applications list.
-- * 'listName' - The name of the AWS Firewall Manager applications list.
-- * 'listUpdateToken' - A unique identifier for each update to the list. When you update the list, the update token must match the token of the current version of the application list. You can retrieve the update token by getting the list.
-- * 'previousAppsList' - A map of previous version numbers to their corresponding @App@ object arrays.
mkAppsListData ::
  -- | 'listName'
  Lude.Text ->
  AppsListData
mkAppsListData pListName_ =
  AppsListData'
    { listUpdateToken = Lude.Nothing,
      listId = Lude.Nothing,
      lastUpdateTime = Lude.Nothing,
      previousAppsList = Lude.Nothing,
      createTime = Lude.Nothing,
      listName = pListName_,
      appsList = Lude.mempty
    }

-- | A unique identifier for each update to the list. When you update the list, the update token must match the token of the current version of the application list. You can retrieve the update token by getting the list.
--
-- /Note:/ Consider using 'listUpdateToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aldListUpdateToken :: Lens.Lens' AppsListData (Lude.Maybe Lude.Text)
aldListUpdateToken = Lens.lens (listUpdateToken :: AppsListData -> Lude.Maybe Lude.Text) (\s a -> s {listUpdateToken = a} :: AppsListData)
{-# DEPRECATED aldListUpdateToken "Use generic-lens or generic-optics with 'listUpdateToken' instead." #-}

-- | The ID of the AWS Firewall Manager applications list.
--
-- /Note:/ Consider using 'listId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aldListId :: Lens.Lens' AppsListData (Lude.Maybe Lude.Text)
aldListId = Lens.lens (listId :: AppsListData -> Lude.Maybe Lude.Text) (\s a -> s {listId = a} :: AppsListData)
{-# DEPRECATED aldListId "Use generic-lens or generic-optics with 'listId' instead." #-}

-- | The time that the AWS Firewall Manager applications list was last updated.
--
-- /Note:/ Consider using 'lastUpdateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aldLastUpdateTime :: Lens.Lens' AppsListData (Lude.Maybe Lude.Timestamp)
aldLastUpdateTime = Lens.lens (lastUpdateTime :: AppsListData -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastUpdateTime = a} :: AppsListData)
{-# DEPRECATED aldLastUpdateTime "Use generic-lens or generic-optics with 'lastUpdateTime' instead." #-}

-- | A map of previous version numbers to their corresponding @App@ object arrays.
--
-- /Note:/ Consider using 'previousAppsList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aldPreviousAppsList :: Lens.Lens' AppsListData (Lude.Maybe (Lude.HashMap Lude.Text ([App])))
aldPreviousAppsList = Lens.lens (previousAppsList :: AppsListData -> Lude.Maybe (Lude.HashMap Lude.Text ([App]))) (\s a -> s {previousAppsList = a} :: AppsListData)
{-# DEPRECATED aldPreviousAppsList "Use generic-lens or generic-optics with 'previousAppsList' instead." #-}

-- | The time that the AWS Firewall Manager applications list was created.
--
-- /Note:/ Consider using 'createTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aldCreateTime :: Lens.Lens' AppsListData (Lude.Maybe Lude.Timestamp)
aldCreateTime = Lens.lens (createTime :: AppsListData -> Lude.Maybe Lude.Timestamp) (\s a -> s {createTime = a} :: AppsListData)
{-# DEPRECATED aldCreateTime "Use generic-lens or generic-optics with 'createTime' instead." #-}

-- | The name of the AWS Firewall Manager applications list.
--
-- /Note:/ Consider using 'listName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aldListName :: Lens.Lens' AppsListData Lude.Text
aldListName = Lens.lens (listName :: AppsListData -> Lude.Text) (\s a -> s {listName = a} :: AppsListData)
{-# DEPRECATED aldListName "Use generic-lens or generic-optics with 'listName' instead." #-}

-- | An array of applications in the AWS Firewall Manager applications list.
--
-- /Note:/ Consider using 'appsList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aldAppsList :: Lens.Lens' AppsListData [App]
aldAppsList = Lens.lens (appsList :: AppsListData -> [App]) (\s a -> s {appsList = a} :: AppsListData)
{-# DEPRECATED aldAppsList "Use generic-lens or generic-optics with 'appsList' instead." #-}

instance Lude.FromJSON AppsListData where
  parseJSON =
    Lude.withObject
      "AppsListData"
      ( \x ->
          AppsListData'
            Lude.<$> (x Lude..:? "ListUpdateToken")
            Lude.<*> (x Lude..:? "ListId")
            Lude.<*> (x Lude..:? "LastUpdateTime")
            Lude.<*> (x Lude..:? "PreviousAppsList" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "CreateTime")
            Lude.<*> (x Lude..: "ListName")
            Lude.<*> (x Lude..:? "AppsList" Lude..!= Lude.mempty)
      )

instance Lude.ToJSON AppsListData where
  toJSON AppsListData' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ListUpdateToken" Lude..=) Lude.<$> listUpdateToken,
            ("ListId" Lude..=) Lude.<$> listId,
            ("LastUpdateTime" Lude..=) Lude.<$> lastUpdateTime,
            ("PreviousAppsList" Lude..=) Lude.<$> previousAppsList,
            ("CreateTime" Lude..=) Lude.<$> createTime,
            Lude.Just ("ListName" Lude..= listName),
            Lude.Just ("AppsList" Lude..= appsList)
          ]
      )
