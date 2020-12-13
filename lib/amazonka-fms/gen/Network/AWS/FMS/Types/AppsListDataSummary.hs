{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FMS.Types.AppsListDataSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.FMS.Types.AppsListDataSummary
  ( AppsListDataSummary (..),

    -- * Smart constructor
    mkAppsListDataSummary,

    -- * Lenses
    aldsListARN,
    aldsAppsList,
    aldsListId,
    aldsListName,
  )
where

import Network.AWS.FMS.Types.App
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Details of the AWS Firewall Manager applications list.
--
-- /See:/ 'mkAppsListDataSummary' smart constructor.
data AppsListDataSummary = AppsListDataSummary'
  { -- | The Amazon Resource Name (ARN) of the applications list.
    listARN :: Lude.Maybe Lude.Text,
    -- | An array of @App@ objects in the AWS Firewall Manager applications list.
    appsList :: Lude.Maybe [App],
    -- | The ID of the applications list.
    listId :: Lude.Maybe Lude.Text,
    -- | The name of the applications list.
    listName :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AppsListDataSummary' with the minimum fields required to make a request.
--
-- * 'listARN' - The Amazon Resource Name (ARN) of the applications list.
-- * 'appsList' - An array of @App@ objects in the AWS Firewall Manager applications list.
-- * 'listId' - The ID of the applications list.
-- * 'listName' - The name of the applications list.
mkAppsListDataSummary ::
  AppsListDataSummary
mkAppsListDataSummary =
  AppsListDataSummary'
    { listARN = Lude.Nothing,
      appsList = Lude.Nothing,
      listId = Lude.Nothing,
      listName = Lude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the applications list.
--
-- /Note:/ Consider using 'listARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aldsListARN :: Lens.Lens' AppsListDataSummary (Lude.Maybe Lude.Text)
aldsListARN = Lens.lens (listARN :: AppsListDataSummary -> Lude.Maybe Lude.Text) (\s a -> s {listARN = a} :: AppsListDataSummary)
{-# DEPRECATED aldsListARN "Use generic-lens or generic-optics with 'listARN' instead." #-}

-- | An array of @App@ objects in the AWS Firewall Manager applications list.
--
-- /Note:/ Consider using 'appsList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aldsAppsList :: Lens.Lens' AppsListDataSummary (Lude.Maybe [App])
aldsAppsList = Lens.lens (appsList :: AppsListDataSummary -> Lude.Maybe [App]) (\s a -> s {appsList = a} :: AppsListDataSummary)
{-# DEPRECATED aldsAppsList "Use generic-lens or generic-optics with 'appsList' instead." #-}

-- | The ID of the applications list.
--
-- /Note:/ Consider using 'listId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aldsListId :: Lens.Lens' AppsListDataSummary (Lude.Maybe Lude.Text)
aldsListId = Lens.lens (listId :: AppsListDataSummary -> Lude.Maybe Lude.Text) (\s a -> s {listId = a} :: AppsListDataSummary)
{-# DEPRECATED aldsListId "Use generic-lens or generic-optics with 'listId' instead." #-}

-- | The name of the applications list.
--
-- /Note:/ Consider using 'listName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aldsListName :: Lens.Lens' AppsListDataSummary (Lude.Maybe Lude.Text)
aldsListName = Lens.lens (listName :: AppsListDataSummary -> Lude.Maybe Lude.Text) (\s a -> s {listName = a} :: AppsListDataSummary)
{-# DEPRECATED aldsListName "Use generic-lens or generic-optics with 'listName' instead." #-}

instance Lude.FromJSON AppsListDataSummary where
  parseJSON =
    Lude.withObject
      "AppsListDataSummary"
      ( \x ->
          AppsListDataSummary'
            Lude.<$> (x Lude..:? "ListArn")
            Lude.<*> (x Lude..:? "AppsList" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "ListId")
            Lude.<*> (x Lude..:? "ListName")
      )
