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
-- Module      : Network.AWS.FMS.Types.ProtocolsListDataSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.FMS.Types.ProtocolsListDataSummary where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Details of the AWS Firewall Manager protocols list.
--
-- /See:/ 'newProtocolsListDataSummary' smart constructor.
data ProtocolsListDataSummary = ProtocolsListDataSummary'
  { -- | The name of the specified protocols list.
    listName :: Core.Maybe Core.Text,
    -- | An array of protocols in the AWS Firewall Manager protocols list.
    protocolsList :: Core.Maybe [Core.Text],
    -- | The Amazon Resource Name (ARN) of the specified protocols list.
    listArn :: Core.Maybe Core.Text,
    -- | The ID of the specified protocols list.
    listId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ProtocolsListDataSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'listName', 'protocolsListDataSummary_listName' - The name of the specified protocols list.
--
-- 'protocolsList', 'protocolsListDataSummary_protocolsList' - An array of protocols in the AWS Firewall Manager protocols list.
--
-- 'listArn', 'protocolsListDataSummary_listArn' - The Amazon Resource Name (ARN) of the specified protocols list.
--
-- 'listId', 'protocolsListDataSummary_listId' - The ID of the specified protocols list.
newProtocolsListDataSummary ::
  ProtocolsListDataSummary
newProtocolsListDataSummary =
  ProtocolsListDataSummary'
    { listName = Core.Nothing,
      protocolsList = Core.Nothing,
      listArn = Core.Nothing,
      listId = Core.Nothing
    }

-- | The name of the specified protocols list.
protocolsListDataSummary_listName :: Lens.Lens' ProtocolsListDataSummary (Core.Maybe Core.Text)
protocolsListDataSummary_listName = Lens.lens (\ProtocolsListDataSummary' {listName} -> listName) (\s@ProtocolsListDataSummary' {} a -> s {listName = a} :: ProtocolsListDataSummary)

-- | An array of protocols in the AWS Firewall Manager protocols list.
protocolsListDataSummary_protocolsList :: Lens.Lens' ProtocolsListDataSummary (Core.Maybe [Core.Text])
protocolsListDataSummary_protocolsList = Lens.lens (\ProtocolsListDataSummary' {protocolsList} -> protocolsList) (\s@ProtocolsListDataSummary' {} a -> s {protocolsList = a} :: ProtocolsListDataSummary) Core.. Lens.mapping Lens._Coerce

-- | The Amazon Resource Name (ARN) of the specified protocols list.
protocolsListDataSummary_listArn :: Lens.Lens' ProtocolsListDataSummary (Core.Maybe Core.Text)
protocolsListDataSummary_listArn = Lens.lens (\ProtocolsListDataSummary' {listArn} -> listArn) (\s@ProtocolsListDataSummary' {} a -> s {listArn = a} :: ProtocolsListDataSummary)

-- | The ID of the specified protocols list.
protocolsListDataSummary_listId :: Lens.Lens' ProtocolsListDataSummary (Core.Maybe Core.Text)
protocolsListDataSummary_listId = Lens.lens (\ProtocolsListDataSummary' {listId} -> listId) (\s@ProtocolsListDataSummary' {} a -> s {listId = a} :: ProtocolsListDataSummary)

instance Core.FromJSON ProtocolsListDataSummary where
  parseJSON =
    Core.withObject
      "ProtocolsListDataSummary"
      ( \x ->
          ProtocolsListDataSummary'
            Core.<$> (x Core..:? "ListName")
            Core.<*> (x Core..:? "ProtocolsList" Core..!= Core.mempty)
            Core.<*> (x Core..:? "ListArn")
            Core.<*> (x Core..:? "ListId")
      )

instance Core.Hashable ProtocolsListDataSummary

instance Core.NFData ProtocolsListDataSummary
