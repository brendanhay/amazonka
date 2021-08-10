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
-- Module      : Network.AWS.EKS.Types.AddonIssue
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EKS.Types.AddonIssue where

import qualified Network.AWS.Core as Core
import Network.AWS.EKS.Types.AddonIssueCode
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | An issue related to an add-on.
--
-- /See:/ 'newAddonIssue' smart constructor.
data AddonIssue = AddonIssue'
  { -- | A message that provides details about the issue and what might cause it.
    message :: Prelude.Maybe Prelude.Text,
    -- | A code that describes the type of issue.
    code :: Prelude.Maybe AddonIssueCode,
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
-- 'message', 'addonIssue_message' - A message that provides details about the issue and what might cause it.
--
-- 'code', 'addonIssue_code' - A code that describes the type of issue.
--
-- 'resourceIds', 'addonIssue_resourceIds' - The resource IDs of the issue.
newAddonIssue ::
  AddonIssue
newAddonIssue =
  AddonIssue'
    { message = Prelude.Nothing,
      code = Prelude.Nothing,
      resourceIds = Prelude.Nothing
    }

-- | A message that provides details about the issue and what might cause it.
addonIssue_message :: Lens.Lens' AddonIssue (Prelude.Maybe Prelude.Text)
addonIssue_message = Lens.lens (\AddonIssue' {message} -> message) (\s@AddonIssue' {} a -> s {message = a} :: AddonIssue)

-- | A code that describes the type of issue.
addonIssue_code :: Lens.Lens' AddonIssue (Prelude.Maybe AddonIssueCode)
addonIssue_code = Lens.lens (\AddonIssue' {code} -> code) (\s@AddonIssue' {} a -> s {code = a} :: AddonIssue)

-- | The resource IDs of the issue.
addonIssue_resourceIds :: Lens.Lens' AddonIssue (Prelude.Maybe [Prelude.Text])
addonIssue_resourceIds = Lens.lens (\AddonIssue' {resourceIds} -> resourceIds) (\s@AddonIssue' {} a -> s {resourceIds = a} :: AddonIssue) Prelude.. Lens.mapping Lens._Coerce

instance Core.FromJSON AddonIssue where
  parseJSON =
    Core.withObject
      "AddonIssue"
      ( \x ->
          AddonIssue'
            Prelude.<$> (x Core..:? "message")
            Prelude.<*> (x Core..:? "code")
            Prelude.<*> (x Core..:? "resourceIds" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable AddonIssue

instance Prelude.NFData AddonIssue
