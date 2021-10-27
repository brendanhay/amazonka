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
-- Module      : Network.AWS.AuditManager.Types.Resource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AuditManager.Types.Resource where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A system asset that is evaluated in an Audit Manager assessment.
--
-- /See:/ 'newResource' smart constructor.
data Resource = Resource'
  { -- | The Amazon Resource Name (ARN) for the specified resource.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The value of the specified resource.
    value :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Resource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'resource_arn' - The Amazon Resource Name (ARN) for the specified resource.
--
-- 'value', 'resource_value' - The value of the specified resource.
newResource ::
  Resource
newResource =
  Resource'
    { arn = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) for the specified resource.
resource_arn :: Lens.Lens' Resource (Prelude.Maybe Prelude.Text)
resource_arn = Lens.lens (\Resource' {arn} -> arn) (\s@Resource' {} a -> s {arn = a} :: Resource)

-- | The value of the specified resource.
resource_value :: Lens.Lens' Resource (Prelude.Maybe Prelude.Text)
resource_value = Lens.lens (\Resource' {value} -> value) (\s@Resource' {} a -> s {value = a} :: Resource)

instance Core.FromJSON Resource where
  parseJSON =
    Core.withObject
      "Resource"
      ( \x ->
          Resource'
            Prelude.<$> (x Core..:? "arn") Prelude.<*> (x Core..:? "value")
      )

instance Prelude.Hashable Resource

instance Prelude.NFData Resource
