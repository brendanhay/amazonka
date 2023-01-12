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
-- Module      : Amazonka.EKS.Types.AddonIssue
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EKS.Types.AddonIssue where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EKS.Types.AddonIssueCode
import qualified Amazonka.Prelude as Prelude

-- | An issue related to an add-on.
--
-- /See:/ 'newAddonIssue' smart constructor.
data AddonIssue = AddonIssue'
  { -- | A code that describes the type of issue.
    code :: Prelude.Maybe AddonIssueCode,
    -- | A message that provides details about the issue and what might cause it.
    message :: Prelude.Maybe Prelude.Text,
    -- | The resource IDs of the issue.
    resourceIds :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AddonIssue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'code', 'addonIssue_code' - A code that describes the type of issue.
--
-- 'message', 'addonIssue_message' - A message that provides details about the issue and what might cause it.
--
-- 'resourceIds', 'addonIssue_resourceIds' - The resource IDs of the issue.
newAddonIssue ::
  AddonIssue
newAddonIssue =
  AddonIssue'
    { code = Prelude.Nothing,
      message = Prelude.Nothing,
      resourceIds = Prelude.Nothing
    }

-- | A code that describes the type of issue.
addonIssue_code :: Lens.Lens' AddonIssue (Prelude.Maybe AddonIssueCode)
addonIssue_code = Lens.lens (\AddonIssue' {code} -> code) (\s@AddonIssue' {} a -> s {code = a} :: AddonIssue)

-- | A message that provides details about the issue and what might cause it.
addonIssue_message :: Lens.Lens' AddonIssue (Prelude.Maybe Prelude.Text)
addonIssue_message = Lens.lens (\AddonIssue' {message} -> message) (\s@AddonIssue' {} a -> s {message = a} :: AddonIssue)

-- | The resource IDs of the issue.
addonIssue_resourceIds :: Lens.Lens' AddonIssue (Prelude.Maybe [Prelude.Text])
addonIssue_resourceIds = Lens.lens (\AddonIssue' {resourceIds} -> resourceIds) (\s@AddonIssue' {} a -> s {resourceIds = a} :: AddonIssue) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON AddonIssue where
  parseJSON =
    Data.withObject
      "AddonIssue"
      ( \x ->
          AddonIssue'
            Prelude.<$> (x Data..:? "code")
            Prelude.<*> (x Data..:? "message")
            Prelude.<*> (x Data..:? "resourceIds" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable AddonIssue where
  hashWithSalt _salt AddonIssue' {..} =
    _salt `Prelude.hashWithSalt` code
      `Prelude.hashWithSalt` message
      `Prelude.hashWithSalt` resourceIds

instance Prelude.NFData AddonIssue where
  rnf AddonIssue' {..} =
    Prelude.rnf code
      `Prelude.seq` Prelude.rnf message
      `Prelude.seq` Prelude.rnf resourceIds
