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
-- Module      : Amazonka.OAM.Types.ListLinksItem
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OAM.Types.ListLinksItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A structure that contains information about one of this source
-- account\'s links to a monitoring account.
--
-- /See:/ 'newListLinksItem' smart constructor.
data ListLinksItem = ListLinksItem'
  { -- | The ARN of the link.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The random ID string that Amazon Web Services generated as part of the
    -- link ARN.
    id :: Prelude.Maybe Prelude.Text,
    -- | The label that was assigned to this link at creation, with the variables
    -- resolved to their actual values.
    label :: Prelude.Maybe Prelude.Text,
    -- | The resource types supported by this link.
    resourceTypes :: Prelude.Maybe [Prelude.Text],
    -- | The ARN of the sink that this link is attached to.
    sinkArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListLinksItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'listLinksItem_arn' - The ARN of the link.
--
-- 'id', 'listLinksItem_id' - The random ID string that Amazon Web Services generated as part of the
-- link ARN.
--
-- 'label', 'listLinksItem_label' - The label that was assigned to this link at creation, with the variables
-- resolved to their actual values.
--
-- 'resourceTypes', 'listLinksItem_resourceTypes' - The resource types supported by this link.
--
-- 'sinkArn', 'listLinksItem_sinkArn' - The ARN of the sink that this link is attached to.
newListLinksItem ::
  ListLinksItem
newListLinksItem =
  ListLinksItem'
    { arn = Prelude.Nothing,
      id = Prelude.Nothing,
      label = Prelude.Nothing,
      resourceTypes = Prelude.Nothing,
      sinkArn = Prelude.Nothing
    }

-- | The ARN of the link.
listLinksItem_arn :: Lens.Lens' ListLinksItem (Prelude.Maybe Prelude.Text)
listLinksItem_arn = Lens.lens (\ListLinksItem' {arn} -> arn) (\s@ListLinksItem' {} a -> s {arn = a} :: ListLinksItem)

-- | The random ID string that Amazon Web Services generated as part of the
-- link ARN.
listLinksItem_id :: Lens.Lens' ListLinksItem (Prelude.Maybe Prelude.Text)
listLinksItem_id = Lens.lens (\ListLinksItem' {id} -> id) (\s@ListLinksItem' {} a -> s {id = a} :: ListLinksItem)

-- | The label that was assigned to this link at creation, with the variables
-- resolved to their actual values.
listLinksItem_label :: Lens.Lens' ListLinksItem (Prelude.Maybe Prelude.Text)
listLinksItem_label = Lens.lens (\ListLinksItem' {label} -> label) (\s@ListLinksItem' {} a -> s {label = a} :: ListLinksItem)

-- | The resource types supported by this link.
listLinksItem_resourceTypes :: Lens.Lens' ListLinksItem (Prelude.Maybe [Prelude.Text])
listLinksItem_resourceTypes = Lens.lens (\ListLinksItem' {resourceTypes} -> resourceTypes) (\s@ListLinksItem' {} a -> s {resourceTypes = a} :: ListLinksItem) Prelude.. Lens.mapping Lens.coerced

-- | The ARN of the sink that this link is attached to.
listLinksItem_sinkArn :: Lens.Lens' ListLinksItem (Prelude.Maybe Prelude.Text)
listLinksItem_sinkArn = Lens.lens (\ListLinksItem' {sinkArn} -> sinkArn) (\s@ListLinksItem' {} a -> s {sinkArn = a} :: ListLinksItem)

instance Data.FromJSON ListLinksItem where
  parseJSON =
    Data.withObject
      "ListLinksItem"
      ( \x ->
          ListLinksItem'
            Prelude.<$> (x Data..:? "Arn")
            Prelude.<*> (x Data..:? "Id")
            Prelude.<*> (x Data..:? "Label")
            Prelude.<*> (x Data..:? "ResourceTypes" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "SinkArn")
      )

instance Prelude.Hashable ListLinksItem where
  hashWithSalt _salt ListLinksItem' {..} =
    _salt `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` label
      `Prelude.hashWithSalt` resourceTypes
      `Prelude.hashWithSalt` sinkArn

instance Prelude.NFData ListLinksItem where
  rnf ListLinksItem' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf label
      `Prelude.seq` Prelude.rnf resourceTypes
      `Prelude.seq` Prelude.rnf sinkArn
