{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FMS.Types.ProtocolsListData
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.FMS.Types.ProtocolsListData
  ( ProtocolsListData (..),

    -- * Smart constructor
    mkProtocolsListData,

    -- * Lenses
    pldProtocolsList,
    pldListUpdateToken,
    pldListId,
    pldListName,
    pldLastUpdateTime,
    pldPreviousProtocolsList,
    pldCreateTime,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An AWS Firewall Manager protocols list.
--
-- /See:/ 'mkProtocolsListData' smart constructor.
data ProtocolsListData = ProtocolsListData'
  { -- | An array of protocols in the AWS Firewall Manager protocols list.
    protocolsList :: [Lude.Text],
    -- | A unique identifier for each update to the list. When you update the list, the update token must match the token of the current version of the application list. You can retrieve the update token by getting the list.
    listUpdateToken :: Lude.Maybe Lude.Text,
    -- | The ID of the AWS Firewall Manager protocols list.
    listId :: Lude.Maybe Lude.Text,
    -- | The name of the AWS Firewall Manager protocols list.
    listName :: Lude.Text,
    -- | The time that the AWS Firewall Manager protocols list was last updated.
    lastUpdateTime :: Lude.Maybe Lude.Timestamp,
    -- | A map of previous version numbers to their corresponding protocol arrays.
    previousProtocolsList :: Lude.Maybe (Lude.HashMap Lude.Text ([Lude.Text])),
    -- | The time that the AWS Firewall Manager protocols list was created.
    createTime :: Lude.Maybe Lude.Timestamp
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ProtocolsListData' with the minimum fields required to make a request.
--
-- * 'protocolsList' - An array of protocols in the AWS Firewall Manager protocols list.
-- * 'listUpdateToken' - A unique identifier for each update to the list. When you update the list, the update token must match the token of the current version of the application list. You can retrieve the update token by getting the list.
-- * 'listId' - The ID of the AWS Firewall Manager protocols list.
-- * 'listName' - The name of the AWS Firewall Manager protocols list.
-- * 'lastUpdateTime' - The time that the AWS Firewall Manager protocols list was last updated.
-- * 'previousProtocolsList' - A map of previous version numbers to their corresponding protocol arrays.
-- * 'createTime' - The time that the AWS Firewall Manager protocols list was created.
mkProtocolsListData ::
  -- | 'listName'
  Lude.Text ->
  ProtocolsListData
mkProtocolsListData pListName_ =
  ProtocolsListData'
    { protocolsList = Lude.mempty,
      listUpdateToken = Lude.Nothing,
      listId = Lude.Nothing,
      listName = pListName_,
      lastUpdateTime = Lude.Nothing,
      previousProtocolsList = Lude.Nothing,
      createTime = Lude.Nothing
    }

-- | An array of protocols in the AWS Firewall Manager protocols list.
--
-- /Note:/ Consider using 'protocolsList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pldProtocolsList :: Lens.Lens' ProtocolsListData [Lude.Text]
pldProtocolsList = Lens.lens (protocolsList :: ProtocolsListData -> [Lude.Text]) (\s a -> s {protocolsList = a} :: ProtocolsListData)
{-# DEPRECATED pldProtocolsList "Use generic-lens or generic-optics with 'protocolsList' instead." #-}

-- | A unique identifier for each update to the list. When you update the list, the update token must match the token of the current version of the application list. You can retrieve the update token by getting the list.
--
-- /Note:/ Consider using 'listUpdateToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pldListUpdateToken :: Lens.Lens' ProtocolsListData (Lude.Maybe Lude.Text)
pldListUpdateToken = Lens.lens (listUpdateToken :: ProtocolsListData -> Lude.Maybe Lude.Text) (\s a -> s {listUpdateToken = a} :: ProtocolsListData)
{-# DEPRECATED pldListUpdateToken "Use generic-lens or generic-optics with 'listUpdateToken' instead." #-}

-- | The ID of the AWS Firewall Manager protocols list.
--
-- /Note:/ Consider using 'listId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pldListId :: Lens.Lens' ProtocolsListData (Lude.Maybe Lude.Text)
pldListId = Lens.lens (listId :: ProtocolsListData -> Lude.Maybe Lude.Text) (\s a -> s {listId = a} :: ProtocolsListData)
{-# DEPRECATED pldListId "Use generic-lens or generic-optics with 'listId' instead." #-}

-- | The name of the AWS Firewall Manager protocols list.
--
-- /Note:/ Consider using 'listName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pldListName :: Lens.Lens' ProtocolsListData Lude.Text
pldListName = Lens.lens (listName :: ProtocolsListData -> Lude.Text) (\s a -> s {listName = a} :: ProtocolsListData)
{-# DEPRECATED pldListName "Use generic-lens or generic-optics with 'listName' instead." #-}

-- | The time that the AWS Firewall Manager protocols list was last updated.
--
-- /Note:/ Consider using 'lastUpdateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pldLastUpdateTime :: Lens.Lens' ProtocolsListData (Lude.Maybe Lude.Timestamp)
pldLastUpdateTime = Lens.lens (lastUpdateTime :: ProtocolsListData -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastUpdateTime = a} :: ProtocolsListData)
{-# DEPRECATED pldLastUpdateTime "Use generic-lens or generic-optics with 'lastUpdateTime' instead." #-}

-- | A map of previous version numbers to their corresponding protocol arrays.
--
-- /Note:/ Consider using 'previousProtocolsList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pldPreviousProtocolsList :: Lens.Lens' ProtocolsListData (Lude.Maybe (Lude.HashMap Lude.Text ([Lude.Text])))
pldPreviousProtocolsList = Lens.lens (previousProtocolsList :: ProtocolsListData -> Lude.Maybe (Lude.HashMap Lude.Text ([Lude.Text]))) (\s a -> s {previousProtocolsList = a} :: ProtocolsListData)
{-# DEPRECATED pldPreviousProtocolsList "Use generic-lens or generic-optics with 'previousProtocolsList' instead." #-}

-- | The time that the AWS Firewall Manager protocols list was created.
--
-- /Note:/ Consider using 'createTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pldCreateTime :: Lens.Lens' ProtocolsListData (Lude.Maybe Lude.Timestamp)
pldCreateTime = Lens.lens (createTime :: ProtocolsListData -> Lude.Maybe Lude.Timestamp) (\s a -> s {createTime = a} :: ProtocolsListData)
{-# DEPRECATED pldCreateTime "Use generic-lens or generic-optics with 'createTime' instead." #-}

instance Lude.FromJSON ProtocolsListData where
  parseJSON =
    Lude.withObject
      "ProtocolsListData"
      ( \x ->
          ProtocolsListData'
            Lude.<$> (x Lude..:? "ProtocolsList" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "ListUpdateToken")
            Lude.<*> (x Lude..:? "ListId")
            Lude.<*> (x Lude..: "ListName")
            Lude.<*> (x Lude..:? "LastUpdateTime")
            Lude.<*> (x Lude..:? "PreviousProtocolsList" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "CreateTime")
      )

instance Lude.ToJSON ProtocolsListData where
  toJSON ProtocolsListData' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ProtocolsList" Lude..= protocolsList),
            ("ListUpdateToken" Lude..=) Lude.<$> listUpdateToken,
            ("ListId" Lude..=) Lude.<$> listId,
            Lude.Just ("ListName" Lude..= listName),
            ("LastUpdateTime" Lude..=) Lude.<$> lastUpdateTime,
            ("PreviousProtocolsList" Lude..=) Lude.<$> previousProtocolsList,
            ("CreateTime" Lude..=) Lude.<$> createTime
          ]
      )
