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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EKS.Types.AddonIssue where

import qualified Amazonka.Core as Core
import Amazonka.EKS.Types.AddonIssueCode
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | An issue related to an add-on.
--
-- /See:/ 'newAddonIssue' smart constructor.
data AddonIssue = AddonIssue'
  { -- | The resource IDs of the issue.
    resourceIds :: Prelude.Maybe [Prelude.Text],
    -- | A code that describes the type of issue.
    code :: Prelude.Maybe AddonIssueCode,
    -- | A message that provides details about the issue and what might cause it.
    message :: Prelude.Maybe Prelude.Text
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
-- 'resourceIds', 'addonIssue_resourceIds' - The resource IDs of the issue.
--
-- 'code', 'addonIssue_code' - A code that describes the type of issue.
--
-- 'message', 'addonIssue_message' - A message that provides details about the issue and what might cause it.
newAddonIssue ::
  AddonIssue
newAddonIssue =
  AddonIssue'
    { resourceIds = Prelude.Nothing,
      code = Prelude.Nothing,
      message = Prelude.Nothing
    }

-- | The resource IDs of the issue.
addonIssue_resourceIds :: Lens.Lens' AddonIssue (Prelude.Maybe [Prelude.Text])
addonIssue_resourceIds = Lens.lens (\AddonIssue' {resourceIds} -> resourceIds) (\s@AddonIssue' {} a -> s {resourceIds = a} :: AddonIssue) Prelude.. Lens.mapping Lens.coerced

-- | A code that describes the type of issue.
addonIssue_code :: Lens.Lens' AddonIssue (Prelude.Maybe AddonIssueCode)
addonIssue_code = Lens.lens (\AddonIssue' {code} -> code) (\s@AddonIssue' {} a -> s {code = a} :: AddonIssue)

-- | A message that provides details about the issue and what might cause it.
addonIssue_message :: Lens.Lens' AddonIssue (Prelude.Maybe Prelude.Text)
addonIssue_message = Lens.lens (\AddonIssue' {message} -> message) (\s@AddonIssue' {} a -> s {message = a} :: AddonIssue)

instance Core.FromJSON AddonIssue where
  parseJSON =
    Core.withObject
      "AddonIssue"
      ( \x ->
          AddonIssue'
            Prelude.<$> (x Core..:? "resourceIds" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "code")
            Prelude.<*> (x Core..:? "message")
      )

instance Prelude.Hashable AddonIssue

instance Prelude.NFData AddonIssue
