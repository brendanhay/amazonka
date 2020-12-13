{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FMS.Types.ProtocolsListDataSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.FMS.Types.ProtocolsListDataSummary
  ( ProtocolsListDataSummary (..),

    -- * Smart constructor
    mkProtocolsListDataSummary,

    -- * Lenses
    pldsProtocolsList,
    pldsListARN,
    pldsListId,
    pldsListName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Details of the AWS Firewall Manager protocols list.
--
-- /See:/ 'mkProtocolsListDataSummary' smart constructor.
data ProtocolsListDataSummary = ProtocolsListDataSummary'
  { -- | An array of protocols in the AWS Firewall Manager protocols list.
    protocolsList :: Lude.Maybe [Lude.Text],
    -- | The Amazon Resource Name (ARN) of the specified protocols list.
    listARN :: Lude.Maybe Lude.Text,
    -- | The ID of the specified protocols list.
    listId :: Lude.Maybe Lude.Text,
    -- | The name of the specified protocols list.
    listName :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ProtocolsListDataSummary' with the minimum fields required to make a request.
--
-- * 'protocolsList' - An array of protocols in the AWS Firewall Manager protocols list.
-- * 'listARN' - The Amazon Resource Name (ARN) of the specified protocols list.
-- * 'listId' - The ID of the specified protocols list.
-- * 'listName' - The name of the specified protocols list.
mkProtocolsListDataSummary ::
  ProtocolsListDataSummary
mkProtocolsListDataSummary =
  ProtocolsListDataSummary'
    { protocolsList = Lude.Nothing,
      listARN = Lude.Nothing,
      listId = Lude.Nothing,
      listName = Lude.Nothing
    }

-- | An array of protocols in the AWS Firewall Manager protocols list.
--
-- /Note:/ Consider using 'protocolsList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pldsProtocolsList :: Lens.Lens' ProtocolsListDataSummary (Lude.Maybe [Lude.Text])
pldsProtocolsList = Lens.lens (protocolsList :: ProtocolsListDataSummary -> Lude.Maybe [Lude.Text]) (\s a -> s {protocolsList = a} :: ProtocolsListDataSummary)
{-# DEPRECATED pldsProtocolsList "Use generic-lens or generic-optics with 'protocolsList' instead." #-}

-- | The Amazon Resource Name (ARN) of the specified protocols list.
--
-- /Note:/ Consider using 'listARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pldsListARN :: Lens.Lens' ProtocolsListDataSummary (Lude.Maybe Lude.Text)
pldsListARN = Lens.lens (listARN :: ProtocolsListDataSummary -> Lude.Maybe Lude.Text) (\s a -> s {listARN = a} :: ProtocolsListDataSummary)
{-# DEPRECATED pldsListARN "Use generic-lens or generic-optics with 'listARN' instead." #-}

-- | The ID of the specified protocols list.
--
-- /Note:/ Consider using 'listId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pldsListId :: Lens.Lens' ProtocolsListDataSummary (Lude.Maybe Lude.Text)
pldsListId = Lens.lens (listId :: ProtocolsListDataSummary -> Lude.Maybe Lude.Text) (\s a -> s {listId = a} :: ProtocolsListDataSummary)
{-# DEPRECATED pldsListId "Use generic-lens or generic-optics with 'listId' instead." #-}

-- | The name of the specified protocols list.
--
-- /Note:/ Consider using 'listName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pldsListName :: Lens.Lens' ProtocolsListDataSummary (Lude.Maybe Lude.Text)
pldsListName = Lens.lens (listName :: ProtocolsListDataSummary -> Lude.Maybe Lude.Text) (\s a -> s {listName = a} :: ProtocolsListDataSummary)
{-# DEPRECATED pldsListName "Use generic-lens or generic-optics with 'listName' instead." #-}

instance Lude.FromJSON ProtocolsListDataSummary where
  parseJSON =
    Lude.withObject
      "ProtocolsListDataSummary"
      ( \x ->
          ProtocolsListDataSummary'
            Lude.<$> (x Lude..:? "ProtocolsList" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "ListArn")
            Lude.<*> (x Lude..:? "ListId")
            Lude.<*> (x Lude..:? "ListName")
      )
