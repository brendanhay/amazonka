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
-- Module      : Amazonka.AuditManager.Types.Resource
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AuditManager.Types.Resource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | A system asset that\'s evaluated in an Audit Manager assessment.
--
-- /See:/ 'newResource' smart constructor.
data Resource = Resource'
  { -- | The Amazon Resource Name (ARN) for the resource.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The evaluation status for a resource that was assessed when collecting
    -- compliance check evidence.
    --
    -- -   Audit Manager classes the resource as non-compliant if Security Hub
    --     reports a /Fail/ result, or if Config reports a /Non-compliant/
    --     result.
    --
    -- -   Audit Manager classes the resource as compliant if Security Hub
    --     reports a /Pass/ result, or if Config reports a /Compliant/ result.
    --
    -- -   If a compliance check isn\'t available or applicable, then no
    --     compliance evaluation can be made for that resource. This is the
    --     case if a resource assessment uses Config or Security Hub as the
    --     underlying data source type, but those services aren\'t enabled.
    --     This is also the case if the resource assessment uses an underlying
    --     data source type that doesn\'t support compliance checks (such as
    --     manual evidence, Amazon Web Services API calls, or CloudTrail).
    complianceCheck :: Prelude.Maybe Prelude.Text,
    -- | The value of the resource.
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
-- 'arn', 'resource_arn' - The Amazon Resource Name (ARN) for the resource.
--
-- 'complianceCheck', 'resource_complianceCheck' - The evaluation status for a resource that was assessed when collecting
-- compliance check evidence.
--
-- -   Audit Manager classes the resource as non-compliant if Security Hub
--     reports a /Fail/ result, or if Config reports a /Non-compliant/
--     result.
--
-- -   Audit Manager classes the resource as compliant if Security Hub
--     reports a /Pass/ result, or if Config reports a /Compliant/ result.
--
-- -   If a compliance check isn\'t available or applicable, then no
--     compliance evaluation can be made for that resource. This is the
--     case if a resource assessment uses Config or Security Hub as the
--     underlying data source type, but those services aren\'t enabled.
--     This is also the case if the resource assessment uses an underlying
--     data source type that doesn\'t support compliance checks (such as
--     manual evidence, Amazon Web Services API calls, or CloudTrail).
--
-- 'value', 'resource_value' - The value of the resource.
newResource ::
  Resource
newResource =
  Resource'
    { arn = Prelude.Nothing,
      complianceCheck = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) for the resource.
resource_arn :: Lens.Lens' Resource (Prelude.Maybe Prelude.Text)
resource_arn = Lens.lens (\Resource' {arn} -> arn) (\s@Resource' {} a -> s {arn = a} :: Resource)

-- | The evaluation status for a resource that was assessed when collecting
-- compliance check evidence.
--
-- -   Audit Manager classes the resource as non-compliant if Security Hub
--     reports a /Fail/ result, or if Config reports a /Non-compliant/
--     result.
--
-- -   Audit Manager classes the resource as compliant if Security Hub
--     reports a /Pass/ result, or if Config reports a /Compliant/ result.
--
-- -   If a compliance check isn\'t available or applicable, then no
--     compliance evaluation can be made for that resource. This is the
--     case if a resource assessment uses Config or Security Hub as the
--     underlying data source type, but those services aren\'t enabled.
--     This is also the case if the resource assessment uses an underlying
--     data source type that doesn\'t support compliance checks (such as
--     manual evidence, Amazon Web Services API calls, or CloudTrail).
resource_complianceCheck :: Lens.Lens' Resource (Prelude.Maybe Prelude.Text)
resource_complianceCheck = Lens.lens (\Resource' {complianceCheck} -> complianceCheck) (\s@Resource' {} a -> s {complianceCheck = a} :: Resource)

-- | The value of the resource.
resource_value :: Lens.Lens' Resource (Prelude.Maybe Prelude.Text)
resource_value = Lens.lens (\Resource' {value} -> value) (\s@Resource' {} a -> s {value = a} :: Resource)

instance Core.FromJSON Resource where
  parseJSON =
    Core.withObject
      "Resource"
      ( \x ->
          Resource'
            Prelude.<$> (x Core..:? "arn")
            Prelude.<*> (x Core..:? "complianceCheck")
            Prelude.<*> (x Core..:? "value")
      )

instance Prelude.Hashable Resource where
  hashWithSalt _salt Resource' {..} =
    _salt `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` complianceCheck
      `Prelude.hashWithSalt` value

instance Prelude.NFData Resource where
  rnf Resource' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf complianceCheck
      `Prelude.seq` Prelude.rnf value
