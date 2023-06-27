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
-- Module      : Amazonka.CloudDirectory.Types.LinkAttributeUpdate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudDirectory.Types.LinkAttributeUpdate where

import Amazonka.CloudDirectory.Types.AttributeKey
import Amazonka.CloudDirectory.Types.LinkAttributeAction
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Structure that contains attribute update information.
--
-- /See:/ 'newLinkAttributeUpdate' smart constructor.
data LinkAttributeUpdate = LinkAttributeUpdate'
  { -- | The action to perform as part of the attribute update.
    attributeAction :: Prelude.Maybe LinkAttributeAction,
    -- | The key of the attribute being updated.
    attributeKey :: Prelude.Maybe AttributeKey
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
        Prelude.Nothing,
      attributeKey = Prelude.Nothing
    }

-- | The action to perform as part of the attribute update.
linkAttributeUpdate_attributeAction :: Lens.Lens' LinkAttributeUpdate (Prelude.Maybe LinkAttributeAction)
linkAttributeUpdate_attributeAction = Lens.lens (\LinkAttributeUpdate' {attributeAction} -> attributeAction) (\s@LinkAttributeUpdate' {} a -> s {attributeAction = a} :: LinkAttributeUpdate)

-- | The key of the attribute being updated.
linkAttributeUpdate_attributeKey :: Lens.Lens' LinkAttributeUpdate (Prelude.Maybe AttributeKey)
linkAttributeUpdate_attributeKey = Lens.lens (\LinkAttributeUpdate' {attributeKey} -> attributeKey) (\s@LinkAttributeUpdate' {} a -> s {attributeKey = a} :: LinkAttributeUpdate)

instance Prelude.Hashable LinkAttributeUpdate where
  hashWithSalt _salt LinkAttributeUpdate' {..} =
    _salt
      `Prelude.hashWithSalt` attributeAction
      `Prelude.hashWithSalt` attributeKey

instance Prelude.NFData LinkAttributeUpdate where
  rnf LinkAttributeUpdate' {..} =
    Prelude.rnf attributeAction
      `Prelude.seq` Prelude.rnf attributeKey

instance Data.ToJSON LinkAttributeUpdate where
  toJSON LinkAttributeUpdate' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AttributeAction" Data..=)
              Prelude.<$> attributeAction,
            ("AttributeKey" Data..=) Prelude.<$> attributeKey
          ]
      )
