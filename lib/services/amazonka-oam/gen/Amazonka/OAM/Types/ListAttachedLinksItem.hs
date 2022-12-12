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
-- Module      : Amazonka.OAM.Types.ListAttachedLinksItem
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OAM.Types.ListAttachedLinksItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A structure that contains information about one link attached to this
-- monitoring account sink.
--
-- /See:/ 'newListAttachedLinksItem' smart constructor.
data ListAttachedLinksItem = ListAttachedLinksItem'
  { -- | The label that was assigned to this link at creation, with the variables
    -- resolved to their actual values.
    label :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the link.
    linkArn :: Prelude.Maybe Prelude.Text,
    -- | The resource types supported by this link.
    resourceTypes :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAttachedLinksItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'label', 'listAttachedLinksItem_label' - The label that was assigned to this link at creation, with the variables
-- resolved to their actual values.
--
-- 'linkArn', 'listAttachedLinksItem_linkArn' - The ARN of the link.
--
-- 'resourceTypes', 'listAttachedLinksItem_resourceTypes' - The resource types supported by this link.
newListAttachedLinksItem ::
  ListAttachedLinksItem
newListAttachedLinksItem =
  ListAttachedLinksItem'
    { label = Prelude.Nothing,
      linkArn = Prelude.Nothing,
      resourceTypes = Prelude.Nothing
    }

-- | The label that was assigned to this link at creation, with the variables
-- resolved to their actual values.
listAttachedLinksItem_label :: Lens.Lens' ListAttachedLinksItem (Prelude.Maybe Prelude.Text)
listAttachedLinksItem_label = Lens.lens (\ListAttachedLinksItem' {label} -> label) (\s@ListAttachedLinksItem' {} a -> s {label = a} :: ListAttachedLinksItem)

-- | The ARN of the link.
listAttachedLinksItem_linkArn :: Lens.Lens' ListAttachedLinksItem (Prelude.Maybe Prelude.Text)
listAttachedLinksItem_linkArn = Lens.lens (\ListAttachedLinksItem' {linkArn} -> linkArn) (\s@ListAttachedLinksItem' {} a -> s {linkArn = a} :: ListAttachedLinksItem)

-- | The resource types supported by this link.
listAttachedLinksItem_resourceTypes :: Lens.Lens' ListAttachedLinksItem (Prelude.Maybe [Prelude.Text])
listAttachedLinksItem_resourceTypes = Lens.lens (\ListAttachedLinksItem' {resourceTypes} -> resourceTypes) (\s@ListAttachedLinksItem' {} a -> s {resourceTypes = a} :: ListAttachedLinksItem) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON ListAttachedLinksItem where
  parseJSON =
    Data.withObject
      "ListAttachedLinksItem"
      ( \x ->
          ListAttachedLinksItem'
            Prelude.<$> (x Data..:? "Label")
            Prelude.<*> (x Data..:? "LinkArn")
            Prelude.<*> (x Data..:? "ResourceTypes" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable ListAttachedLinksItem where
  hashWithSalt _salt ListAttachedLinksItem' {..} =
    _salt `Prelude.hashWithSalt` label
      `Prelude.hashWithSalt` linkArn
      `Prelude.hashWithSalt` resourceTypes

instance Prelude.NFData ListAttachedLinksItem where
  rnf ListAttachedLinksItem' {..} =
    Prelude.rnf label
      `Prelude.seq` Prelude.rnf linkArn
      `Prelude.seq` Prelude.rnf resourceTypes
