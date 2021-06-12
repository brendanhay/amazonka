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
-- Module      : Network.AWS.CloudDirectory.Types.LinkAttributeUpdate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.LinkAttributeUpdate where

import Network.AWS.CloudDirectory.Types.AttributeKey
import Network.AWS.CloudDirectory.Types.LinkAttributeAction
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Structure that contains attribute update information.
--
-- /See:/ 'newLinkAttributeUpdate' smart constructor.
data LinkAttributeUpdate = LinkAttributeUpdate'
  { -- | The action to perform as part of the attribute update.
    attributeAction :: Core.Maybe LinkAttributeAction,
    -- | The key of the attribute being updated.
    attributeKey :: Core.Maybe AttributeKey
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'LinkAttributeUpdate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attributeAction', 'linkAttributeUpdate_attributeAction' - The action to perform as part of the attribute update.
--
-- 'attributeKey', 'linkAttributeUpdate_attributeKey' - The key of the attribute being updated.
newLinkAttributeUpdate ::
  LinkAttributeUpdate
newLinkAttributeUpdate =
  LinkAttributeUpdate'
    { attributeAction =
        Core.Nothing,
      attributeKey = Core.Nothing
    }

-- | The action to perform as part of the attribute update.
linkAttributeUpdate_attributeAction :: Lens.Lens' LinkAttributeUpdate (Core.Maybe LinkAttributeAction)
linkAttributeUpdate_attributeAction = Lens.lens (\LinkAttributeUpdate' {attributeAction} -> attributeAction) (\s@LinkAttributeUpdate' {} a -> s {attributeAction = a} :: LinkAttributeUpdate)

-- | The key of the attribute being updated.
linkAttributeUpdate_attributeKey :: Lens.Lens' LinkAttributeUpdate (Core.Maybe AttributeKey)
linkAttributeUpdate_attributeKey = Lens.lens (\LinkAttributeUpdate' {attributeKey} -> attributeKey) (\s@LinkAttributeUpdate' {} a -> s {attributeKey = a} :: LinkAttributeUpdate)

instance Core.Hashable LinkAttributeUpdate

instance Core.NFData LinkAttributeUpdate

instance Core.ToJSON LinkAttributeUpdate where
  toJSON LinkAttributeUpdate' {..} =
    Core.object
      ( Core.catMaybes
          [ ("AttributeAction" Core..=)
              Core.<$> attributeAction,
            ("AttributeKey" Core..=) Core.<$> attributeKey
          ]
      )
