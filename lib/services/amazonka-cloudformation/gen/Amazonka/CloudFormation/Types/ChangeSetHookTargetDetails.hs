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
-- Module      : Amazonka.CloudFormation.Types.ChangeSetHookTargetDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFormation.Types.ChangeSetHookTargetDetails where

import Amazonka.CloudFormation.Types.ChangeSetHookResourceTargetDetails
import Amazonka.CloudFormation.Types.HookTargetType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Specifies target details for an activated hook.
--
-- /See:/ 'newChangeSetHookTargetDetails' smart constructor.
data ChangeSetHookTargetDetails = ChangeSetHookTargetDetails'
  { -- | The name of the type.
    targetType :: Prelude.Maybe HookTargetType,
    -- | Required if @TargetType@ is @RESOURCE@.
    resourceTargetDetails :: Prelude.Maybe ChangeSetHookResourceTargetDetails
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ChangeSetHookTargetDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'targetType', 'changeSetHookTargetDetails_targetType' - The name of the type.
--
-- 'resourceTargetDetails', 'changeSetHookTargetDetails_resourceTargetDetails' - Required if @TargetType@ is @RESOURCE@.
newChangeSetHookTargetDetails ::
  ChangeSetHookTargetDetails
newChangeSetHookTargetDetails =
  ChangeSetHookTargetDetails'
    { targetType =
        Prelude.Nothing,
      resourceTargetDetails = Prelude.Nothing
    }

-- | The name of the type.
changeSetHookTargetDetails_targetType :: Lens.Lens' ChangeSetHookTargetDetails (Prelude.Maybe HookTargetType)
changeSetHookTargetDetails_targetType = Lens.lens (\ChangeSetHookTargetDetails' {targetType} -> targetType) (\s@ChangeSetHookTargetDetails' {} a -> s {targetType = a} :: ChangeSetHookTargetDetails)

-- | Required if @TargetType@ is @RESOURCE@.
changeSetHookTargetDetails_resourceTargetDetails :: Lens.Lens' ChangeSetHookTargetDetails (Prelude.Maybe ChangeSetHookResourceTargetDetails)
changeSetHookTargetDetails_resourceTargetDetails = Lens.lens (\ChangeSetHookTargetDetails' {resourceTargetDetails} -> resourceTargetDetails) (\s@ChangeSetHookTargetDetails' {} a -> s {resourceTargetDetails = a} :: ChangeSetHookTargetDetails)

instance Core.FromXML ChangeSetHookTargetDetails where
  parseXML x =
    ChangeSetHookTargetDetails'
      Prelude.<$> (x Core..@? "TargetType")
      Prelude.<*> (x Core..@? "ResourceTargetDetails")

instance Prelude.Hashable ChangeSetHookTargetDetails where
  hashWithSalt _salt ChangeSetHookTargetDetails' {..} =
    _salt `Prelude.hashWithSalt` targetType
      `Prelude.hashWithSalt` resourceTargetDetails

instance Prelude.NFData ChangeSetHookTargetDetails where
  rnf ChangeSetHookTargetDetails' {..} =
    Prelude.rnf targetType
      `Prelude.seq` Prelude.rnf resourceTargetDetails
